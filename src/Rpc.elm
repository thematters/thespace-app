port module Rpc exposing
    ( RpcResult(..)
    , approveAllBalance
    , collectUbi
    , decodeMessage
    , defaultRpcErrorCode
    , getAccount
    , getBlockNumber
    , getColorHistory
    , getDeltas
    , getInitMap
    , getLatestColorEvents
    , getLatestDeltaCids
    , getLatestPriceEvents
    , getLatestTaxEvents
    , getLatestTransferEvents
    , getLatestUbiEvents
    , getMintTax
    , getOwnPixels
    , getPixel
    , getTaxRate
    , getTokenInfo
    , getTreasuryShare
    , openSocket
    , reinitApp
    , requestAccount
    , rpcSubs
    , setPixel
    , switchNetwork
    , watchColor
    , watchDefault
    , watchNewHeads
    , watchPrice
    , watchTax
    , watchTransfer
    , watchUbi
    )

import BigInt exposing (BigInt)
import Config exposing (contracts, maxInt256, rpcSocketAddress, topics)
import Config.Env.Util exposing (RpcProvider)
import Contract.ERC20 as ERC20
import Contract.Registry as Registry
import Contract.Snapper as Snapper
import Contract.Space as Space
    exposing
        ( colorDecoder
        , colorLogsDecoder
        , priceDecoder
        , priceLogsDecoder
        , taxDecoder
        , taxLogsDecoder
        , transferDecoder
        , transferLogsDecoder
        , ubiDecoder
        , ubiLogsDecoder
        )
import Contract.Util exposing (unsafeBigIntToInt)
import Data
    exposing
        ( BlockNumber
        , Index
        , OwnPixelsResultPage
        , Pixel
        , Price
        , RpcErrorData
        , RpcErrorKind(..)
        , TaxRate
        , TokenInfo
        , TokenInfoKind(..)
        , WalletDetail
        , WalletInfo(..)
        , accTax
        , cidToSnapshotUri
        , intToHex
        , safeColorId
        )
import Env exposing (env)
import Eth.Decode as ED
import Eth.Encode as EE
import Eth.Types exposing (Address, BlockId(..), Call, Hex)
import Eth.Units exposing (EthUnit(..))
import Eth.Utils exposing (add0x, addressToString, toAddress, unsafeToHex)
import Hex
import Http
import Iso8601
import Json.Decode as D
import Json.Encode as E exposing (Value)
import Model.Playback exposing (deltaDataDecoder)
import Msg exposing (Msg(..))



-- Ports


port walletOut : Value -> Cmd msg


port walletIn : (Value -> msg) -> Sub msg


port openRpcSocket : Value -> Cmd msg


port rpcSocketOut : Value -> Cmd msg


port rpcSocketIn : (Value -> msg) -> Sub msg


port rpcSocketControl : (String -> msg) -> Sub msg



-- Data Types


type RpcResult
    = RpcInitMap Snapper.Snapshot
    | RpcNewHeadsSubId SubId
    | RpcColorSubId SubId
    | RpcPriceSubId SubId
    | RpcTransferSubId SubId
    | RpcTaxSubId SubId
    | RpcUbiSubId SubId
    | RpcDefaultSubId SubId
    | RpcNewHead BlockNumber
    | RpcTaxRate TaxRate
    | RpcTreasuryShare Space.TreasuryShare
    | RpcMintTax Price
    | RpcLatestColorLog (List Space.ColorEvent)
    | RpcLatestPriceLog (List Space.PriceEvent)
    | RpcLatestTransferLog (List Space.TransferEvent)
    | RpcLatestTaxLog (List Space.TaxEvent)
    | RpcLatestUbiLog (List Space.UbiEvent)
    | RpcColorEvent Space.ColorEvent
    | RpcPriceEvent Space.PriceEvent
    | RpcTransferEvent Space.TransferEvent
    | RpcTaxEvent Space.TaxEvent
    | RpcUbiEvent Space.UbiEvent
    | RpcRegistryTransferEvent Registry.RegistryTransferEvent
    | RpcPixel Pixel
    | RpcOwnPixels OwnPixelsResultPage
    | RpcTokenInfo TokenInfo
    | RpcDeltaCids (List Snapper.Cid)
    | RpcError RpcErrorData


type alias SubId =
    Int


type alias TreasuryShare =
    Price


type alias RpcCallData =
    { method : String
    , params : List Value
    , id : MessageId
    }


type MessageId
    = GetInitMap
    | LatestPlaybackDeltaCids
    | WatchNewHeads
    | WatchColor
    | WatchPrice
    | WatchTransfer
    | WatchTax
    | WatchUbi
    | WatchDefault
    | GetTaxRate
    | GetTreasuryShare
    | GetMintTax
    | GetBlockNumber
    | LatestColorEvents
    | LatestPriceEvents
    | LatestTransferEvents
    | LatestTaxEvents
    | LatestUbiEvents
    | GetPixel
    | GetOwnPixels Int
    | GetBalance Address
    | GetAllowance Address



-- Helpers


toPixel : Maybe TaxRate -> Maybe BlockNumber -> Space.GetPixel -> Pixel
toPixel taxRate bkNum pixelBigInt =
    let
        lastTaxBK =
            let
                lbk =
                    pixelBigInt.lastTaxBK |> unsafeBigIntToInt
            in
            if lbk == 0 then
                bkNum |> Maybe.withDefault 0

            else
                lbk
    in
    { index = pixelBigInt.id |> unsafeBigIntToInt
    , owner = pixelBigInt.owner
    , price = pixelBigInt.price
    , color = pixelBigInt.color |> unsafeBigIntToInt
    , ubi = pixelBigInt.ubi
    , lastTaxBK = lastTaxBK
    , tax = accTax pixelBigInt.owner pixelBigInt.price lastTaxBK bkNum taxRate
    , ownerBalance = Nothing
    , ownerAllowance = Nothing
    }


toPixelsByOwner : Maybe TaxRate -> Maybe BlockNumber -> Space.GetPixelsByOwner -> OwnPixelsResultPage
toPixelsByOwner taxRate bkNum pixelsByOwner =
    { total = pixelsByOwner.total |> unsafeBigIntToInt
    , limit = pixelsByOwner.limit |> unsafeBigIntToInt
    , offset = pixelsByOwner.offset |> unsafeBigIntToInt
    , pixels = List.map (toPixel taxRate bkNum) pixelsByOwner.pixels
    }


toBalanceTokenInfo : Address -> Price -> TokenInfo
toBalanceTokenInfo addr amt =
    { kind = BalanceTokenInfo
    , address = addr
    , amount = amt
    }


toAllowanceTokenInfo : Address -> Price -> TokenInfo
toAllowanceTokenInfo addr amt =
    { kind = AllowanceTokenInfo
    , address = addr
    , amount = amt
    }


defaultRpcErrorCode : Int
defaultRpcErrorCode =
    -33000


defaultRpcErrorData : String -> RpcErrorData
defaultRpcErrorData msg =
    { kind = RpcUnknownError -- we may special treat some errors
    , code = defaultRpcErrorCode
    , message = msg
    }


defaultRpcError : String -> RpcResult
defaultRpcError msg =
    RpcError <| defaultRpcErrorData msg



-- Encoders / Decoders


messageIdEncoder : MessageId -> Value
messageIdEncoder msgId =
    -- We don't want to write string literals all over the place.
    -- This way at least they are all in these two msgId encode/decode thingy.
    case msgId of
        GetPixel ->
            E.string "p"

        GetBalance addr ->
            E.string <| "blc-" ++ addressToString addr

        GetAllowance addr ->
            E.string <| "alw-" ++ addressToString addr

        GetOwnPixels offset ->
            E.string <| "ops-" ++ String.fromInt offset

        GetInitMap ->
            E.string "imap"

        LatestPlaybackDeltaCids ->
            E.string "pb"

        GetBlockNumber ->
            E.string "bk"

        WatchNewHeads ->
            E.string "bsub"

        WatchColor ->
            E.string "csub"

        WatchPrice ->
            E.string "psub"

        WatchTransfer ->
            E.string "tsub"

        WatchTax ->
            E.string "tasub"

        WatchUbi ->
            E.string "usub"

        WatchDefault ->
            E.string "dsub"

        GetTaxRate ->
            E.string "tr"

        GetTreasuryShare ->
            E.string "tys"

        GetMintTax ->
            E.string "mt"

        LatestColorEvents ->
            E.string "cs"

        LatestPriceEvents ->
            E.string "ps"

        LatestTransferEvents ->
            E.string "ts"

        LatestTaxEvents ->
            E.string "tas"

        LatestUbiEvents ->
            E.string "us"


messageIdDecoder : D.Decoder MessageId
messageIdDecoder =
    let
        toMessageId msgIdStr =
            case msgIdStr of
                "p" ->
                    GetPixel

                "imap" ->
                    GetInitMap

                "pb" ->
                    LatestPlaybackDeltaCids

                "bk" ->
                    GetBlockNumber

                "bsub" ->
                    WatchNewHeads

                "csub" ->
                    WatchColor

                "psub" ->
                    WatchPrice

                "tsub" ->
                    WatchTransfer

                "tasub" ->
                    WatchTax

                "usub" ->
                    WatchUbi

                "dsub" ->
                    WatchDefault

                "tr" ->
                    GetTaxRate

                "tys" ->
                    GetTreasuryShare

                "mt" ->
                    GetMintTax

                "cs" ->
                    LatestColorEvents

                "ps" ->
                    LatestPriceEvents

                "ts" ->
                    LatestTransferEvents

                "tas" ->
                    LatestTaxEvents

                "us" ->
                    LatestUbiEvents

                s ->
                    if String.startsWith "blc-" s then
                        case toAddress <| String.dropLeft 4 s of
                            Ok addr ->
                                GetBalance addr

                            Err _ ->
                                GetBlockNumber

                    else if String.startsWith "alw-" s then
                        case toAddress <| String.dropLeft 4 s of
                            Ok addr ->
                                GetAllowance addr

                            Err _ ->
                                GetBlockNumber

                    else if String.startsWith "ops-" s then
                        case String.toInt <| String.dropLeft 4 s of
                            Just offset ->
                                GetOwnPixels offset

                            Nothing ->
                                GetBlockNumber

                    else
                        GetBlockNumber
    in
    D.map toMessageId D.string


resultDecoder : MessageId -> Maybe TaxRate -> Maybe BlockNumber -> D.Decoder RpcResult
resultDecoder id taxRate blockNum =
    let
        res =
            D.field "result"

        resHex =
            res ED.hexInt

        resBigInt =
            res ED.bigInt
    in
    case id of
        GetInitMap ->
            res Snapper.snapshotDecoder
                |> D.map RpcInitMap

        LatestPlaybackDeltaCids ->
            res Snapper.deltaLogsDecoder
                |> D.map RpcDeltaCids

        WatchNewHeads ->
            resHex
                |> D.map RpcNewHeadsSubId

        WatchColor ->
            resHex
                |> D.map RpcColorSubId

        WatchPrice ->
            resHex
                |> D.map RpcPriceSubId

        WatchTransfer ->
            resHex
                |> D.map RpcTransferSubId

        WatchTax ->
            resHex
                |> D.map RpcTaxSubId

        WatchUbi ->
            resHex
                |> D.map RpcUbiSubId

        WatchDefault ->
            resHex
                |> D.map RpcDefaultSubId

        GetTaxRate ->
            resBigInt
                |> D.map RpcTaxRate

        GetTreasuryShare ->
            resBigInt
                |> D.map RpcTreasuryShare

        GetMintTax ->
            resBigInt
                |> D.map RpcMintTax

        GetBlockNumber ->
            resHex
                |> D.map RpcNewHead

        LatestColorEvents ->
            res colorLogsDecoder
                |> D.map RpcLatestColorLog

        LatestPriceEvents ->
            res priceLogsDecoder
                |> D.map RpcLatestPriceLog

        LatestTransferEvents ->
            res transferLogsDecoder
                |> D.map RpcLatestTransferLog

        LatestTaxEvents ->
            res taxLogsDecoder
                |> D.map RpcLatestTaxLog

        LatestUbiEvents ->
            res ubiLogsDecoder
                |> D.map RpcLatestUbiLog

        GetBalance addr ->
            resBigInt
                |> D.map (toBalanceTokenInfo addr >> RpcTokenInfo)

        GetAllowance addr ->
            resBigInt
                |> D.map (toAllowanceTokenInfo addr >> RpcTokenInfo)

        GetPixel ->
            res Space.getPixelDecoder
                |> D.map (toPixel taxRate blockNum >> RpcPixel)

        GetOwnPixels _ ->
            res Space.getPixelsByOwnerDecoder
                |> D.map (toPixelsByOwner taxRate blockNum >> RpcOwnPixels)


newHeadDecoder : D.Decoder BlockNumber
newHeadDecoder =
    D.at [ "params", "result", "number" ] ED.hexInt


walletDecoder : D.Decoder WalletDetail
walletDecoder =
    D.map4 WalletDetail
        (D.field "chainId" ED.hexInt)
        (D.field "address" (D.nullable ED.address))
        (D.field "balance" (D.nullable ED.bigInt))
        (D.field "allowance" (D.nullable ED.bigInt))


errorDecoder : D.Decoder RpcErrorData
errorDecoder =
    D.map3 RpcErrorData
        (D.succeed RpcUnknownError)
        (D.at [ "error", "code" ] D.int)
        (D.at [ "error", "message" ] D.string)



-- Encode / Decode Functions


encodeMessage : RpcCallData -> Value
encodeMessage msg =
    E.object
        [ ( "method", E.string msg.method )
        , ( "params", E.list (\v -> v) msg.params )
        , ( "id", messageIdEncoder msg.id )
        , ( "jsonrpc", E.string "2.0" )
        ]


decodeMessage : Maybe TaxRate -> Maybe BlockNumber -> Value -> RpcResult
decodeMessage taxRate bkNum msg =
    case D.decodeValue (D.field "id" messageIdDecoder) msg of
        Ok id ->
            -- all pull results have an "id" field
            decodeResult id taxRate bkNum msg

        Err _ ->
            decodeEvent msg


decodeResult : MessageId -> Maybe TaxRate -> Maybe BlockNumber -> Value -> RpcResult
decodeResult id taxRate bkNum msg =
    let
        handleError =
            D.decodeValue (errorDecoder |> D.map RpcError) msg
                |> Result.withDefault (defaultRpcError "Unknown result.")
    in
    D.decodeValue (resultDecoder id taxRate bkNum) msg
        |> Result.withDefault handleError


decodeEvent : Value -> RpcResult
decodeEvent v =
    let
        eventDecoder =
            D.at [ "params", "result", "topics" ] (D.index 0 ED.hex)
                |> D.andThen decodeEventBySubId

        decodeEventBySubId subId =
            let
                res =
                    D.at [ "params", "result" ]
            in
            if subId == topics.color then
                res colorDecoder
                    |> D.map RpcColorEvent

            else if subId == topics.price then
                res priceDecoder
                    |> D.map RpcPriceEvent

            else if subId == topics.transfer then
                res transferDecoder
                    |> D.map RpcTransferEvent

            else if subId == topics.tax then
                res taxDecoder
                    |> D.map RpcTaxEvent

            else if subId == topics.ubi then
                res ubiDecoder
                    |> D.map RpcUbiEvent

            else if subId == topics.registryTransfer then
                res Registry.transferDecoder
                    |> D.map RpcRegistryTransferEvent

            else
                D.fail "Unknown event"

        decodeNewHead =
            D.decodeValue newHeadDecoder v
                |> Result.map RpcNewHead
                |> Result.withDefault (defaultRpcError "Unknown event.")
    in
    D.decodeValue eventDecoder v
        -- other than color/price/transfer/tax/ubi, we only have newHead to
        -- deal with, so this seems cleaner.
        |> Result.withDefault decodeNewHead



-- API Helpers


send : RpcCallData -> Cmd msg
send msg =
    msg |> encodeMessage |> rpcSocketOut


call : MessageId -> Call a -> Cmd msg
call msgId params =
    send
        { method = "eth_call"
        , id = msgId
        , params = [ EE.txCall params ]
        }


callAt : BlockNumber -> MessageId -> Call a -> Cmd msg
callAt bk msgId params =
    send
        { method = "eth_call"
        , id = msgId
        , params = [ EE.txCall params, EE.blockId <| BlockNum bk ]
        }


watchEvent : MessageId -> Address -> List (Maybe Hex) -> Cmd msg
watchEvent id address topics =
    send
        { method = "eth_subscribe"
        , id = id
        , params =
            [ E.string "logs"
            , E.object
                [ ( "address", EE.address address )
                , ( "topics", EE.topicsList topics )
                ]
            ]
        }


getLatestEvents : MessageId -> Address -> Hex -> Int -> Cmd msg
getLatestEvents msgId contract topic bk =
    send
        { method = "eth_getLogs"
        , id = msgId
        , params =
            [ E.object
                [ ( "address", EE.address contract )
                , ( "topics", EE.topicsList [ Just topic ] )
                , ( "fromBlock", EE.hexInt bk )
                , ( "toBlock", E.string "latest" )
                ]
            ]
        }


getTaxConfig : MessageId -> BigInt -> Cmd msg
getTaxConfig msgId optionIndex =
    call msgId <| Space.taxConfig contracts.registry optionIndex


approveAllowance : Address -> Price -> Cmd msg
approveAllowance address amount =
    let
        params =
            ERC20.approve contracts.erc20 address contracts.registry amount

        getBalanceParams =
            ERC20.balanceOf contracts.erc20 address
    in
    E.object
        [ ( "method", E.string "approveAllowance" )
        , ( "params", E.list EE.txCall [ params ] )
        , ( "getBalanceParams", E.list EE.txCall [ getBalanceParams ] )
        ]
        |> walletOut



-- Provider APIs


openSocket : Cmd msg
openSocket =
    openRpcSocket <|
        E.object
            [ ( "debug", E.bool env.debug )
            , ( "env", E.string env.name )
            , ( "rpc", E.string rpcSocketAddress )
            ]


getInitMap : Cmd msg
getInitMap =
    call GetInitMap <| Snapper.latestSnapshot contracts.snapper


getLatestDeltaCids : BlockNumber -> Cmd msg
getLatestDeltaCids deltaBkNum =
    send
        { method = "eth_getLogs"
        , id = LatestPlaybackDeltaCids
        , params =
            [ E.object
                [ ( "address", EE.address contracts.snapper )
                , ( "topics"
                  , EE.topicsList
                        [ Just topics.delta
                        , Just <| unsafeToHex <| intToHex 0 -- regionId 0
                        ]
                  )
                , ( "fromBlock", EE.hexInt deltaBkNum )
                , ( "toBlock", E.string "latest" )
                ]
            ]
        }


getLatestColorEvents : Int -> Cmd msg
getLatestColorEvents bk =
    getLatestEvents LatestColorEvents contracts.registry topics.color bk


getTaxRate : Cmd msg
getTaxRate =
    getTaxConfig GetTaxRate Space.taxRateIndex


getTreasuryShare : Cmd msg
getTreasuryShare =
    getTaxConfig GetTreasuryShare Space.treasuryShareIndex


getMintTax : Cmd msg
getMintTax =
    getTaxConfig GetMintTax Space.mintTaxIndex


watchNewHeads : Cmd msg
watchNewHeads =
    send
        { method = "eth_subscribe"
        , id = WatchNewHeads
        , params = [ E.string "newHeads" ]
        }


getBlockNumber : Cmd msg
getBlockNumber =
    send { method = "eth_blockNumber", id = GetBlockNumber, params = [] }


watchColor : Cmd msg
watchColor =
    watchEvent WatchColor contracts.registry [ Just topics.color ]


watchPrice : Cmd msg
watchPrice =
    watchEvent WatchPrice contracts.registry [ Just topics.price ]


watchTransfer : Cmd msg
watchTransfer =
    watchEvent WatchTransfer contracts.registry [ Just topics.transfer ]


zeroAddressTopic : Hex
zeroAddressTopic =
    unsafeToHex <| "0x" ++ String.repeat 64 "0"


watchDefault : Cmd msg
watchDefault =
    watchEvent WatchDefault
        contracts.registry
        [ Just topics.registryTransfer, Nothing, Just zeroAddressTopic ]


watchTax : Cmd msg
watchTax =
    watchEvent WatchTax contracts.registry [ Just topics.tax ]


watchUbi : Cmd msg
watchUbi =
    watchEvent WatchUbi contracts.registry [ Just topics.ubi ]


getColorHistory : Cmd msg
getColorHistory =
    Cmd.none


getPixel : Int -> Cmd msg
getPixel index =
    call GetPixel <| Space.getPixel contracts.space <| BigInt.fromInt index


getOwnPixels : Maybe BlockNumber -> Address -> Int -> Int -> Cmd msg
getOwnPixels bk_ owner limit_ offset_ =
    let
        limit =
            BigInt.fromInt limit_

        offset =
            BigInt.fromInt offset_

        params =
            Space.getPixelsByOwner contracts.space owner limit offset

        msgId =
            GetOwnPixels offset_
    in
    case bk_ of
        Just bk ->
            callAt bk msgId params

        Nothing ->
            call msgId params



-- Wallet APIs


reinitApp : Cmd msg
reinitApp =
    E.object [ ( "method", E.string "reinitApp" ) ] |> walletOut


setPixel : Address -> Int -> Price -> Price -> Int -> Cmd msg
setPixel address index bidPrice newPrice newColor =
    let
        idx =
            BigInt.fromInt index

        color =
            newColor |> safeColorId |> BigInt.fromInt

        params =
            Space.setPixel contracts.space address idx bidPrice newPrice color
    in
    E.object
        [ ( "method", E.string "setPixel" )
        , ( "params", E.list EE.txCall [ params ] )
        , ( "index", E.int index ) -- for nicer error message, potentially
        ]
        |> walletOut


collectUbi : Address -> Index -> Cmd msg
collectUbi address idx =
    let
        params =
            Space.withdrawUbi contracts.space address <| BigInt.fromInt idx
    in
    E.object
        [ ( "method", E.string "collectUbi" )
        , ( "params", E.list EE.txCall [ params ] )
        ]
        |> walletOut


getAccount : Cmd msg
getAccount =
    E.object [ ( "method", E.string "wallet_getAccounts" ) ] |> walletOut


requestAccount : Cmd msg
requestAccount =
    E.object [ ( "method", E.string "wallet_requestAccounts" ) ] |> walletOut


getBalance : Address -> Cmd msg
getBalance address =
    let
        msgId =
            GetBalance address
    in
    call msgId <| ERC20.balanceOf contracts.erc20 address


getAllowance : Address -> Cmd msg
getAllowance address =
    let
        msgId =
            GetAllowance address
    in
    call msgId <| ERC20.allowance contracts.erc20 address contracts.registry


getTokenInfo : Address -> Cmd msg
getTokenInfo address =
    Cmd.batch [ getBalance address, getAllowance address ]


approveAllBalance : Address -> Cmd msg
approveAllBalance address =
    approveAllowance address maxInt256


switchNetwork : RpcProvider -> Cmd msg
switchNetwork rpc =
    let
        currency =
            [ ( "name", E.string rpc.nativeCurrency.name )
            , ( "symbol", E.string rpc.nativeCurrency.symbol )
            , ( "decimals", E.int rpc.nativeCurrency.decimals )
            ]

        params =
            [ ( "chainId", E.string (rpc.chainId |> Hex.toString |> add0x) )
            , ( "chainName", E.string rpc.chainName )
            , ( "nativeCurrency", E.object currency )
            , ( "rpcUrls", E.list E.string rpc.rpcUrls )
            , ( "blockExplorerUrls", E.list E.string rpc.blockExplorerUrls )
            ]
    in
    E.object
        [ ( "method", E.string "wallet_addEthereumChain" )
        , ( "params", E.list E.object [ params ] )
        ]
        |> walletOut



-- Http API


getDeltas : List Snapper.Cid -> Cmd Msg
getDeltas cids =
    Cmd.batch <|
        List.map
            (\cid ->
                Http.get
                    { url = cidToSnapshotUri cid
                    , expect =
                        Http.expectJson DeltaRecieved <|
                            deltaDataDecoder cid
                    }
            )
            cids



-- these three are for development convenience


getLatestPriceEvents : Int -> Cmd msg
getLatestPriceEvents bk =
    getLatestEvents LatestPriceEvents contracts.registry topics.price bk


getLatestTransferEvents : Int -> Cmd msg
getLatestTransferEvents bk =
    getLatestEvents LatestTransferEvents contracts.registry topics.transfer bk


getLatestTaxEvents : Int -> Cmd msg
getLatestTaxEvents bk =
    getLatestEvents LatestTaxEvents contracts.registry topics.tax bk


getLatestUbiEvents : Int -> Cmd msg
getLatestUbiEvents bk =
    getLatestEvents LatestUbiEvents contracts.registry topics.ubi bk



-- Subscription


rpcSubs : List (Sub Msg)
rpcSubs =
    [ rpcSocketIn RpcMessageRecieved
    , rpcSocketControl handleRpcSocketControlMessage
    , walletIn handleWalletMessage
    ]


handleRpcSocketControlMessage : String -> Msg
handleRpcSocketControlMessage s =
    case s of
        "opened" ->
            RpcSocketOpened

        "closed" ->
            RpcSocketClosed

        "reconnecting" ->
            RpcSocketReconnecting

        "reconnected" ->
            RpcSocketReconnected

        _ ->
            NoOp


handleWalletMessage : Value -> Msg
handleWalletMessage =
    D.decodeValue walletMessageDecoder >> Result.withDefault NoOp


walletMessageDecoder : D.Decoder Msg
walletMessageDecoder =
    D.field "type" D.string |> D.andThen walletMessageDataDecoder


walletMessageDataDecoder : String -> D.Decoder Msg
walletMessageDataDecoder t =
    case t of
        "no-wallet" ->
            D.succeed <| WalletInfoChanged NoWallet

        "wallet-locked" ->
            D.succeed WalletLocked

        "wallet" ->
            D.field "data" walletDecoder
                |> D.map (Wallet >> WalletInfoChanged)

        "allowance" ->
            D.field "balance" (D.nullable ED.bigInt)
                |> D.map
                    (\i ->
                        case i of
                            Just blc ->
                                AllowanceApproved blc

                            Nothing ->
                                AllowanceRejected
                    )

        "tx-send" ->
            D.field "data" D.int |> D.map TxConfirmed

        "tx-underpriced" ->
            D.field "data" D.int |> D.map TxUnderPriced

        "tx-rejected" ->
            D.field "data" D.int |> D.map TxRejected

        "income" ->
            D.field "data" D.bool
                |> D.map
                    (\i ->
                        if i then
                            IncomeCollected

                        else
                            IncomeCollectFailed
                    )

        _ ->
            D.fail <| "Wallet: unknown " ++ t ++ " message."
