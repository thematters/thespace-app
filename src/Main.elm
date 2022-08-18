module Main exposing (main)

import Browser
import Browser.Events as E
import Browser.Navigation as Nav
import Canvas as C
import Config exposing (..)
import Data exposing (..)
import Dict
import InfiniteList
import Json.Decode as D exposing (Decoder)
import Json.Encode exposing (Value)
import Model exposing (..)
import Model.Assets as A
import Model.Playback as PB
import Msg exposing (Msg(..))
import Rpc exposing (RpcResult(..))
import Url exposing (Url)
import View


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = View.view
        , subscriptions = subscriptions
        , onUrlChange =
            --\i ->
            --let
            --    _ =
            --        Debug.log "urlChanged" i
            --in
            --NoOp
            \_ -> NoOp
        , onUrlRequest =
            --\i ->
            --    let
            --        _ =
            --            Debug.log "urlRequest" i
            --    in
            --    NoOp
            \_ -> NoOp
        }



-- Init


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init { winW, winH } url key =
    --let
    --    _ =
    --        Debug.log "initUrl" url
    --in
    ( { initModel | winSize = ( winW, winH ), urlKey = Just key }
    , Rpc.openSocket
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        --Rpc
        RpcSocketOpened ->
            ( model, Rpc.getInitMap )

        MapSnapshotInited ->
            case model.mapStatus of
                MSLoading ->
                    ( { model | mapStatus = MSSnapshotInited }, Cmd.none )

                MSSnapshotInited ->
                    ( model, Cmd.none )

                MSLatestColorLoaded colors ->
                    ( { model | mapStatus = MSInited }
                    , C.initLatestColors colors
                    )

                MSInited ->
                    ( model, Cmd.none )

        RpcMessageRecieved message ->
            handleRpcMessageRecieved model message

        RpcSocketClosed ->
            let
                notif =
                    ConnectionLostNotif
            in
            ( { model | notif = Just notif }, Cmd.none )

        RpcSocketReconnecting ->
            let
                notif =
                    ReconnectingNotif
            in
            ( { model | notif = Just notif }, Cmd.none )

        RpcSocketReconnected ->
            ( { model | notif = Nothing }
            , let
                cmds =
                    [ Rpc.watchNewHeads
                    , Rpc.watchColor
                    , Rpc.watchPrice
                    , Rpc.watchTransfer
                    , Rpc.watchTax
                    , Rpc.watchUbi
                    , Rpc.watchDefault
                    ]
              in
              case model.blockNumber of
                Nothing ->
                    Cmd.batch cmds

                Just bk ->
                    Cmd.batch <| Rpc.getLatestColorEvents bk :: cmds
            )

        ReInitApp ->
            ( model, Rpc.reinitApp )

        -- Wallet
        ConnectMetaMask ->
            ( model, Rpc.requestAccount )

        SwitchWalletNetwork network ->
            ( model, Rpc.switchNetwork network )

        WalletLocked ->
            ( { model | wallet = LockedWallet }, Cmd.none )

        WalletInfoChanged wallet ->
            ( { model
                | wallet = wallet
                , sidebarInfLists =
                    { actsInfList = InfiniteList.init
                    , assetsInfList = InfiniteList.init
                    }
                , assets = A.init
                , queue = Dict.empty
              }
            , case wallet of
                Wallet { address } ->
                    case address of
                        Nothing ->
                            Cmd.none

                        Just addr ->
                            Rpc.getTokenInfo addr

                _ ->
                    Cmd.none
            )

        RequestApproveAllBalance ->
            ( model
            , case model.wallet of
                Wallet { address } ->
                    case address of
                        Nothing ->
                            Cmd.none

                        Just addr ->
                            Rpc.approveAllBalance addr

                _ ->
                    Cmd.none
            )

        AllowanceApproved blc ->
            ( case model.wallet of
                Wallet ({ address } as wallet) ->
                    case address of
                        Just _ ->
                            let
                                newWallet =
                                    Wallet
                                        { wallet
                                            | balance = Just blc
                                            , allowance = Just maxInt256
                                        }
                            in
                            { model | wallet = newWallet }

                        Nothing ->
                            model

                _ ->
                    model
            , Cmd.none
            )

        AllowanceRejected ->
            ( model, Cmd.none )

        TxConfirmed idx ->
            ( let
                m =
                    { model
                        | acts = TxSendAct idx :: model.acts
                    }
              in
              case model.selectCell of
                LoadedCell ( pxl, WaitForWallet, e ) ->
                    if pxl.index == idx then
                        { m | selectCell = LoadedCell ( pxl, WaitForChain, e ) }

                    else
                        m

                _ ->
                    model
            , Cmd.none
            )

        TxUnderPriced idx ->
            let
                err =
                    { kind = RpcUnderPricedError idx
                    , code = Rpc.defaultRpcErrorCode -- we dont't care
                    , message = "underpriced" -- only shown in debug env
                    }

                m =
                    { model | acts = ActError err :: model.acts }
            in
            ( case model.selectCell of
                LoadedCell ( pxl, WaitForWallet, _ ) ->
                    if pxl.index == idx then
                        { m
                            | selectCell =
                                LoadedCell ( pxl, WaitForWallet, Just err )
                        }

                    else
                        m

                _ ->
                    m
            , Cmd.none
            )

        TxRejected idx ->
            let
                m =
                    { model | queue = model.queue |> Dict.remove idx }
            in
            ( case model.selectCell of
                LoadedCell ( { index } as pxl, WaitForWallet, e ) ->
                    if index == idx then
                        { m | selectCell = LoadedCell ( pxl, SetNewPrice, e ) }

                    else
                        m

                _ ->
                    m
            , Cmd.none
            )

        -- User Interactions
        WindowResize winSize ->
            let
                m =
                    { model
                        | winSize = winSize
                        , miniMapMode = autoMiniMapMode winSize
                        , sidebarMode = autoSiebarMode winSize model.sidebarMode
                        , sidebarInfLists =
                            { actsInfList = InfiniteList.init
                            , assetsInfList = InfiniteList.init
                            }
                    }
                        |> resetModel
            in
            ( m
            , Cmd.batch
                [ C.reset m.canvas m.winSize
                , clearUrlParams model.urlKey
                ]
            )

        ZoomIn ->
            handleZoom model In model.cellPos

        ZoomOut ->
            handleZoom model Out model.cellPos

        ZoomInCenter ->
            let
                centerPos =
                    centerCell model.canvas model.winSize
            in
            handleZoom model In centerPos

        ZoomOutCenter ->
            let
                centerPos =
                    centerCell model.canvas model.winSize
            in
            handleZoom model Out centerPos

        ZoomReset ->
            let
                m =
                    model |> resetModel
            in
            ( m
            , Cmd.batch [ C.transform m.canvas, clearUrlParams model.urlKey ]
            )

        MapMouseDown xy ->
            let
                m =
                    model |> removeSelectCell
            in
            ( { m
                | dragging = MapDragging xy
                , pagePos = xy
                , cellPos = posToCell m.canvas xy
              }
            , clearUrlParams m.urlKey
            )

        MapTouchDown xy ->
            let
                m =
                    model |> removeSelectCell
            in
            ( if m.pinch == Nothing then
                { m
                    | dragging = MapDragging xy
                    , pagePos = xy
                    , cellPos = posToCell m.canvas xy
                }

              else
                m
            , clearUrlParams m.urlKey
            )

        MapPinchDown dis ->
            ( { model | pinch = Just dis }, Cmd.none )

        MapPinchChange dis ->
            let
                threshhold =
                    5

                zoom d1 d2 =
                    if d2 - d1 > threshhold then
                        Just Out

                    else if d1 - d2 > threshhold then
                        Just In

                    else
                        Nothing

                m =
                    { model | pinch = Just dis }
            in
            case zoom dis <| Maybe.withDefault 0 <| model.pinch of
                Just direction ->
                    let
                        cc =
                            centerCell model.canvas model.winSize
                    in
                    handleZoom m direction cc

                Nothing ->
                    ( m, Cmd.none )

        MiniMapMouseDown xy ->
            ( { model | dragging = MiniMapDragging xy }, Cmd.none )

        MouseUp xy ->
            let
                m =
                    { model | pinch = Nothing }
            in
            case m.dragging of
                NotDragging ->
                    handleDraggingEnd m

                MiniMapDragging _ ->
                    handleDraggingEnd m

                MapDragging dgpos_ ->
                    let
                        dragEnough dgpos =
                            let
                                delta =
                                    10

                                dx =
                                    xy.x - dgpos.x |> abs

                                dy =
                                    xy.y - dgpos.y |> abs
                            in
                            dx > delta || dy > delta

                        stillAsClick dgpos =
                            not <| dragEnough dgpos
                    in
                    if cellInMap m.cellPos && stillAsClick dgpos_ then
                        handleSelectCell m m.cellPos

                    else
                        handleDraggingEnd model

        TouchMove xy ->
            if model.pinch == Nothing then
                let
                    m =
                        { model
                            | pagePos = xy
                            , cellPos = posToCell model.canvas xy
                        }
                in
                case model.dragging of
                    MapDragging _ ->
                        handleMapDragging m <| positionDelta xy model.pagePos

                    _ ->
                        ( m, Cmd.none )

            else
                ( model, Cmd.none )

        MouseMove xy ->
            let
                m =
                    { model
                        | pagePos = xy
                        , cellPos = posToCell model.canvas xy
                    }
            in
            case model.dragging of
                NotDragging ->
                    ( m, Cmd.none )

                MapDragging _ ->
                    handleMapDragging m <| positionDelta xy model.pagePos

                MiniMapDragging _ ->
                    handleMiniMapDragging m <| positionDelta xy model.pagePos

        -- Cell Ops
        SwitchTaxUbiMode mode ->
            ( { model | cellModalMode = mode }, Cmd.none )

        SelectCell ( idx, centerToCell ) ->
            let
                cvs =
                    if centerToCell then
                        C.centerToCellTransform
                            model.winSize
                            model.canvas.zoom
                            idx

                    else
                        model.canvas
            in
            ( { model
                | canvas = cvs
                , cellPos = indexToCell idx
                , selectCell = LoadingCell idx
              }
            , Cmd.batch
                [ C.transform cvs
                , Rpc.getPixel idx
                , setSelectCellUrl model.urlKey <| indexToCell idx
                ]
            )

        ClearSelectedCell ->
            ( { model | selectCell = NoCell }, clearUrlParams model.urlKey )

        TickColor cid ->
            let
                input =
                    model.input
            in
            ( { model | input = { input | newColorId = cid } }, Cmd.none )

        EnterPrice p ->
            let
                input =
                    model.input
            in
            ( { model | input = { input | newPrice = p } }, Cmd.none )

        PixelOpForward ->
            ( case model.selectCell of
                LoadedCell ( pxl, PickNewColor, e ) ->
                    let
                        input =
                            model.input
                    in
                    { model
                        | selectCell = LoadedCell ( pxl, SetNewPrice, e )
                        , input = { input | quotePrice = pxl.price }
                    }

                _ ->
                    model
            , Cmd.none
            )

        PixelOpBack ->
            ( case model.selectCell of
                LoadedCell ( pxl, SetNewPrice, e ) ->
                    { model | selectCell = LoadedCell ( pxl, PickNewColor, e ) }

                _ ->
                    model
            , Cmd.none
            )

        BidPixel ( bidPrice, newPrice, newColorId ) ->
            case ( model.selectCell, model.wallet ) of
                ( LoadedCell ( { index } as pxl, _, e ), Wallet { address } ) ->
                    case address of
                        Nothing ->
                            ( model, Cmd.none )

                        Just addr ->
                            ( { model
                                | selectCell =
                                    LoadedCell ( pxl, WaitForWallet, e )
                                , queue =
                                    model.queue |> Dict.insert index newColorId
                              }
                            , Rpc.setPixel
                                addr
                                index
                                bidPrice
                                newPrice
                                newColorId
                            )

                _ ->
                    ( model, Cmd.none )

        CollectIncome idx ->
            ( model
            , case model.wallet of
                Wallet { address } ->
                    case address of
                        Just addr ->
                            Rpc.collectUbi addr idx

                        Nothing ->
                            Cmd.none

                _ ->
                    Cmd.none
            )

        IncomeCollected ->
            ( model
            , case model.wallet of
                Wallet { address } ->
                    case address of
                        Just addr ->
                            Rpc.getTokenInfo addr

                        Nothing ->
                            Cmd.none

                _ ->
                    Cmd.none
            )

        IncomeCollectFailed ->
            ( model, Cmd.none )

        SyncQuotePrice ->
            ( case model.selectCell of
                LoadedCell ( pxl, _, _ ) ->
                    let
                        input =
                            model.input
                    in
                    { model | input = { input | quotePrice = pxl.price } }

                _ ->
                    model
            , Cmd.none
            )

        -- Sidebar
        SidebarModeSwitch mode ->
            let
                ( _, infoType ) =
                    model.sidebarMode
            in
            ( { model | sidebarMode = ( mode, infoType ) }, Cmd.none )

        ScrollActivity actsInfList ->
            let
                infLists =
                    model.sidebarInfLists
            in
            ( { model
                | sidebarInfLists = { infLists | actsInfList = actsInfList }
              }
            , Cmd.none
            )

        ScrollAssets assetsInfList ->
            let
                infLists =
                    model.sidebarInfLists
            in
            ( { model
                | sidebarInfLists = { infLists | assetsInfList = assetsInfList }
              }
            , Cmd.none
            )

        SidebarInfoSwitch mode ->
            let
                newModel =
                    { model
                        | sidebarMode = ( Tuple.first model.sidebarMode, mode )
                    }
            in
            case mode of
                AssetsManager ->
                    handleAssets newModel <| A.load newModel.blockNumber

                _ ->
                    ( newModel, Cmd.none )

        LoadAssets ->
            case model.sidebarMode of
                ( _, AssetsManager ) ->
                    handleAssets model <| A.load model.blockNumber

                _ ->
                    ( model, Cmd.none )

        RefreshAssets ->
            case model.sidebarMode of
                ( _, AssetsManager ) ->
                    handleAssets model <| A.load model.blockNumber

                _ ->
                    ( model, Cmd.none )

        SortAssets rank ->
            ( { model | assets = model.assets |> A.sort rank }, Cmd.none )

        -- MiniMap
        MiniMapModeChange CollapsedMiniMap ->
            ( { model | miniMapMode = CollapsedMiniMap }, Cmd.none )

        MiniMapModeChange BirdeyeMiniMap ->
            ( { model | miniMapMode = BirdeyeMiniMap }, C.redrawMiniMap )

        -- App Mode Change
        AppModeChange Loading ->
            ( model, Cmd.none )

        AppModeChange Realtime ->
            case model.mode of
                Loading ->
                    ( { model
                        | mode = Realtime
                        , sidebarMode =
                            autoSiebarMode model.winSize model.sidebarMode
                        , miniMapMode = autoMiniMapMode model.winSize
                        , notif = Nothing
                      }
                    , Cmd.none
                    )

                Realtime ->
                    ( model, Cmd.none )

                Playback ->
                    handlePlayback model PB.exit

        -- Playback
        AppModeChange Playback ->
            case model.mode of
                Realtime ->
                    handlePlayback model PB.enter

                _ ->
                    ( model, Cmd.none )

        DeltaRecieved jsonData ->
            handlePlayback model <| PB.addDeltaData jsonData

        PlaybackSnapshotReady ->
            handlePlayback model PB.setSnapshotReady

        PlaybackRewindTimeline colorStrIds ->
            handlePlayback model <| PB.setRewindTimeline colorStrIds

        PlaybackPlay ->
            handlePlayback model PB.play

        PlaybackPause ->
            handlePlayback model PB.pause

        PlaybackTicked ->
            handlePlayback model PB.tick

        PlaybackSlide i ->
            handlePlayback model <| PB.jumpTo i

        PlaybackCircleSpeed ->
            handlePlayback model PB.speedUp

        NoOp ->
            ( model, Cmd.none )



-- User Interaction Handlers


handleZoom : Model -> ZoomDirection -> Cell -> ( Model, Cmd msg )
handleZoom m direction cell =
    let
        delta =
            case direction of
                In ->
                    1

                Out ->
                    -1

        zoom =
            m.canvas.zoom + delta |> clamp minZoom maxZoom

        canvas =
            m.canvas |> C.zoomTransform m.winSize cell zoom
    in
    ( { m | canvas = canvas }, C.transform canvas )


handleDraggingEnd : Model -> ( Model, Cmd msg )
handleDraggingEnd model =
    ( model |> removeMouseFlags, Cmd.none )


handleSelectCell : Model -> Cell -> ( Model, Cmd Msg )
handleSelectCell model cell =
    let
        m =
            model |> removeMouseFlags

        zoom =
            m.canvas.zoom |> clamp clickZoom maxZoom

        cvs =
            let
                cvsScale =
                    m.canvas |> C.zoomTransform m.winSize m.cellPos zoom

                pos =
                    cellToPos cvsScale cell

                ( wW, wH ) =
                    model.winSize |> sizeToFloatSize

                ( dx, dxRight ) =
                    ( pos.x, wW - pos.x )

                ( dy, dyBottom ) =
                    ( pos.y, wH - pos.y )

                ( cW, cH, edge ) =
                    ( cellModalWidth, cellModalEdge, cellModalEdge )

                dxAdjust =
                    if max dx dxRight >= cW then
                        0

                    else if dx > dxRight then
                        cW - dx

                    else
                        dxRight - cW - edge

                dyAdjust =
                    if max dy dyBottom >= cH then
                        0

                    else if dy > dyBottom then
                        cH - dy

                    else
                        dyBottom - cH - edge
            in
            cvsScale |> C.freeMoveTransform dxAdjust dyAdjust

        index =
            cellToIndex cell
    in
    ( { m | canvas = cvs, selectCell = LoadingCell index }
    , Cmd.batch
        [ C.transform cvs
        , Rpc.getPixel index
        , setSelectCellUrl model.urlKey cell
        ]
    )


handleMapDragging : Model -> PositionDelta -> ( Model, Cmd Msg )
handleMapDragging model { dx, dy } =
    let
        canvas =
            model.canvas |> C.moveTransform dx dy model.winSize
    in
    ( { model | canvas = canvas }
    , C.transform canvas
    )


handleMiniMapDragging : Model -> PositionDelta -> ( Model, Cmd msg )
handleMiniMapDragging m { dx, dy } =
    let
        ( mW, mH ) =
            mapSize |> sizeToFloatSize

        cvs =
            m.canvas

        dx_ =
            -dx |> scale miniMapWidth (mW * cvs.zoom)

        dy_ =
            -dy |> scale miniMapHeight (mH * cvs.zoom)

        ( mmdx, mmdy ) =
            ( cvs.dx + dx_, cvs.dy + dy_ )
                |> C.moveClampToEdgeTransform cvs.zoom m.winSize

        canvas =
            { cvs | dx = mmdx, dy = mmdy, zoom = cvs.zoom }
    in
    ( { m | canvas = canvas }, C.transform canvas )


resetModel : Model -> Model
resetModel model =
    { model | canvas = C.resetTransform model.winSize }
        |> removeMouseFlags
        |> removeSelectCell


removeMouseFlags : Model -> Model
removeMouseFlags model =
    { model | dragging = NotDragging }


removeSelectCell : Model -> Model
removeSelectCell model =
    { model | selectCell = NoCell }



-- Assets Hanlder


handleAssets : Model -> A.Handler -> ( Model, Cmd Msg )
handleAssets model handler =
    let
        ( newAssets, action ) =
            model.assets |> handler model.wallet
    in
    ( { model | assets = newAssets }, handleAssetsAction action )


handleAssetsAction : A.Action -> Cmd Msg
handleAssetsAction action =
    case action of
        A.LoadPage ( addr, bk, start ) ->
            Rpc.getAssets addr bk getAssetsPageLen start

        A.Actions actions ->
            Cmd.batch <| List.map handleAssetsAction actions

        A.NoAction ->
            Cmd.none



-- Playback Handler


handlePlayback : Model -> PB.Handler -> ( Model, Cmd Msg )
handlePlayback model handler =
    let
        ( newPlayback, action ) =
            handler model.playback
    in
    handlePlaybackAction { model | playback = newPlayback } action


handlePlaybackAction : Model -> PB.Action -> ( Model, Cmd Msg )
handlePlaybackAction model action =
    case action of
        PB.LoadDeltas cids ->
            ( model, Rpc.getDeltas cids )

        PB.InitSnapshot snapshot ->
            ( model, C.initPlayback <| cidToSnapshotUri snapshot )

        PB.BuildRewindTimeline timeline ->
            ( { model
                | mode = Playback
                , miniMapMode = CollapsedMiniMap
              }
            , C.playbackRewindTimeline timeline
            )

        PB.EnterPlayback ->
            ( model
            , Cmd.batch [ C.enterPlayback, setPlaybackUrl model.urlKey ]
            )

        PB.Forward cs ->
            ( model, C.forward cs )

        PB.Rewind cs ->
            ( model, C.rewind cs )

        PB.PlayAgain ->
            ( model, C.playAgain )

        PB.SetSpeed speed ->
            ( model, C.playbackChangeSpeed speed )

        PB.ExitPlayback ->
            ( { model
                | mode = Realtime
                , sidebarMode = autoSiebarMode model.winSize model.sidebarMode
                , miniMapMode = autoMiniMapMode model.winSize
              }
            , Cmd.batch [ C.endPlayback, clearUrlParams model.urlKey ]
            )

        PB.NoAction ->
            ( model, Cmd.none )


clearUrlParams : Maybe Nav.Key -> Cmd Msg
clearUrlParams urlKey =
    case urlKey of
        Nothing ->
            Cmd.none

        Just key ->
            Nav.replaceUrl key "/"


setPlaybackUrl : Maybe Nav.Key -> Cmd Msg
setPlaybackUrl urlKey =
    case urlKey of
        Nothing ->
            Cmd.none

        Just key ->
            Nav.replaceUrl key "#playback"


setSelectCellUrl : Maybe Nav.Key -> Cell -> Cmd Msg
setSelectCellUrl urlKey cell =
    case urlKey of
        Nothing ->
            Cmd.none

        Just key ->
            Nav.replaceUrl key <| "#" ++ cellString cell



-- Rpc Message Handler


handleRpcMessageRecieved : Model -> Value -> ( Model, Cmd Msg )
handleRpcMessageRecieved model msg =
    case Rpc.decodeMessage model.taxInfo.taxRate model.blockNumber msg of
        RpcInitMap snapshot ->
            let
                canvas =
                    let
                        resetTrans =
                            C.resetTransform model.winSize
                    in
                    case model.selectCell of
                        NoCell ->
                            resetTrans

                        LoadedCell _ ->
                            resetTrans

                        LoadingCell index ->
                            if not <| validIndex index then
                                resetTrans

                            else
                                C.centerToCellTransform
                                    model.winSize
                                    model.canvas.zoom
                                    index
            in
            ( { model | canvas = canvas, selectCell = NoCell }
            , Cmd.batch <|
                [ C.initMapSnapshot
                    canvas
                    model.winSize
                  <|
                    cidToSnapshotUri snapshot.cid
                , Rpc.getLatestColorEvents snapshot.blockNumber
                , Rpc.watchNewHeads
                , Rpc.watchColor
                , Rpc.watchPrice
                , Rpc.watchTransfer
                , Rpc.watchTax
                , Rpc.watchUbi
                , Rpc.watchDefault
                , Rpc.getAccount
                , Rpc.getTaxRate
                , Rpc.getMintTax
                , Rpc.getTreasuryShare
                , Rpc.getLatestDeltaCids snapshot.blockNumber
                ]
                    ++ (if Config.debug then
                            [ Rpc.getLatestPriceEvents snapshot.blockNumber
                            , Rpc.getLatestTransferEvents snapshot.blockNumber
                            , Rpc.getLatestTaxEvents snapshot.blockNumber
                            , Rpc.getLatestUbiEvents snapshot.blockNumber
                            ]

                        else
                            []
                       )
            )

        RpcNewHeadsSubId subId ->
            let
                watchIds =
                    model.watchIds
            in
            ( { model | watchIds = { watchIds | newHeads = Just subId } }
            , Cmd.none
            )

        RpcColorSubId subId ->
            let
                watchIds =
                    model.watchIds
            in
            ( { model | watchIds = { watchIds | color = Just subId } }
            , Cmd.none
            )

        RpcPriceSubId subId ->
            let
                watchIds =
                    model.watchIds
            in
            ( { model | watchIds = { watchIds | price = Just subId } }
            , Cmd.none
            )

        RpcTransferSubId subId ->
            let
                watchIds =
                    model.watchIds
            in
            ( { model | watchIds = { watchIds | transfer = Just subId } }
            , Cmd.none
            )

        RpcTaxSubId subId ->
            let
                watchIds =
                    model.watchIds
            in
            ( { model | watchIds = { watchIds | tax = Just subId } }
            , Cmd.none
            )

        RpcUbiSubId subId ->
            let
                watchIds =
                    model.watchIds
            in
            ( { model | watchIds = { watchIds | ubi = Just subId } }
            , Cmd.none
            )

        RpcDefaultSubId subId ->
            let
                watchIds =
                    model.watchIds
            in
            ( { model | watchIds = { watchIds | default = Just subId } }
            , Cmd.none
            )

        RpcNewHead bkNum ->
            let
                newBK =
                    Just bkNum

                m =
                    { model | blockNumber = newBK }

                newModel =
                    case model.selectCell of
                        LoadedCell ( { price, lastTaxBK } as pxl, s, e ) ->
                            let
                                newTax =
                                    accTax pxl.owner
                                        price
                                        lastTaxBK
                                        newBK
                                        model.taxInfo.taxRate

                                newCell =
                                    { pxl | tax = newTax }
                            in
                            { m | selectCell = LoadedCell ( newCell, s, e ) }

                        _ ->
                            m
            in
            ( newModel, Cmd.none )

        RpcTaxRate taxRate ->
            let
                taxInfo =
                    model.taxInfo
            in
            ( { model | taxInfo = { taxInfo | taxRate = Just taxRate } }
            , Cmd.none
            )

        RpcTreasuryShare tshare ->
            let
                taxInfo =
                    model.taxInfo
            in
            ( { model | taxInfo = { taxInfo | treasuryShare = Just tshare } }
            , Cmd.none
            )

        RpcMintTax mintTax ->
            let
                taxInfo =
                    model.taxInfo
            in
            ( { model | taxInfo = { taxInfo | mintTax = Just mintTax } }
            , Cmd.none
            )

        RpcLatestColorLog cs ->
            let
                m =
                    { model | playback = PB.addColorEvents cs model.playback }

                newActs =
                    if Config.debug then
                        let
                            acts =
                                List.map ColorAct cs
                        in
                        (model.acts ++ acts) |> sortActs

                    else
                        model.acts
            in
            case model.mapStatus of
                MSLoading ->
                    ( { m
                        | mapStatus = MSLatestColorLoaded cs
                        , acts = newActs
                      }
                    , Cmd.none
                    )

                MSSnapshotInited ->
                    ( { m | mapStatus = MSInited, acts = newActs }
                    , C.initLatestColors cs
                    )

                MSLatestColorLoaded _ ->
                    ( m, Cmd.none )

                MSInited ->
                    ( m, Cmd.none )

        RpcLatestPriceLog ps ->
            if Config.debug then
                let
                    acts =
                        ps |> List.map PriceAct
                in
                ( { model | acts = (model.acts ++ acts) |> sortActs }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        RpcLatestTransferLog ts ->
            if Config.debug then
                let
                    acts =
                        ts |> List.map TransferAct
                in
                ( { model | acts = (model.acts ++ acts) |> sortActs }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        RpcLatestTaxLog ts ->
            if Config.debug then
                let
                    acts =
                        ts |> List.map TaxAct
                in
                ( { model | acts = (model.acts ++ acts) |> sortActs }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        RpcLatestUbiLog us ->
            if Config.debug then
                let
                    acts =
                        us |> List.map UbiAct
                in
                ( { model | acts = (model.acts ++ acts) |> sortActs }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        RpcColorEvent c ->
            let
                m =
                    { model
                        | acts = ColorAct c :: model.acts
                        , queue = model.queue |> Dict.remove c.index
                        , assets = model.assets |> A.updateByColor c
                        , playback = model.playback |> PB.addColorEvent c
                    }
            in
            ( case m.selectCell of
                LoadedCell ( { index } as pxl, _, e ) ->
                    if c.index == index then
                        let
                            cell =
                                { pxl | color = c.color }
                        in
                        { m
                            | selectCell = LoadedCell ( cell, PickNewColor, e )
                        }

                    else
                        m

                _ ->
                    m
            , C.update [ c ]
            )

        RpcPriceEvent p ->
            let
                m =
                    { model
                        | acts = PriceAct p :: model.acts
                        , queue = model.queue |> Dict.remove p.index
                        , assets = model.assets |> A.updateByPrice p
                    }
            in
            ( case model.selectCell of
                LoadedCell ( { index } as pxl, _, e ) ->
                    if p.index == index then
                        let
                            newCell =
                                { pxl | price = p.price }
                        in
                        { m
                            | selectCell =
                                LoadedCell ( newCell, PickNewColor, e )
                        }

                    else
                        m

                _ ->
                    m
            , Cmd.none
            )

        RpcTransferEvent t ->
            let
                m =
                    { model
                        | acts = TransferAct t :: model.acts
                        , queue = model.queue |> Dict.remove t.index
                        , assets =
                            model.assets |> A.updateByTransfer model.wallet t
                    }

                m2 =
                    case model.selectCell of
                        LoadedCell ( { index } as pxl, WaitForChain, e ) ->
                            if t.index == index then
                                case model.wallet of
                                    Wallet { address } ->
                                        if address == Just t.to then
                                            let
                                                newCell =
                                                    { pxl | owner = t.to }

                                                newSelectCell =
                                                    LoadedCell
                                                        ( newCell
                                                        , PickNewColor
                                                        , e
                                                        )
                                            in
                                            { m | selectCell = newSelectCell }

                                        else
                                            m

                                    _ ->
                                        m

                            else
                                m

                        LoadedCell ( { index } as pxl, s, e ) ->
                            if t.index == index then
                                let
                                    cell =
                                        { pxl | owner = t.to }
                                in
                                { m | selectCell = LoadedCell ( cell, s, e ) }

                            else
                                m

                        _ ->
                            m

                cmd =
                    case model.wallet of
                        Wallet { address } ->
                            if address == Just t.from then
                                Rpc.getTokenInfo t.from

                            else if address == Just t.to then
                                Cmd.batch
                                    [ Rpc.getTokenInfo t.to
                                    , Rpc.getPixel t.index
                                    ]

                            else
                                Cmd.none

                        _ ->
                            Cmd.none
            in
            ( m2, cmd )

        RpcTaxEvent tax ->
            let
                m =
                    { model | acts = TaxAct tax :: model.acts }

                newModel =
                    case model.selectCell of
                        LoadedCell ( { index } as pxl, s, e ) ->
                            let
                                newUbi =
                                    accUbi pxl.ubi mapSize tax.amount

                                newCell =
                                    { pxl | ubi = newUbi }

                                m2 =
                                    { m
                                        | selectCell =
                                            LoadedCell ( newCell, s, e )
                                    }
                            in
                            if index == tax.index then
                                let
                                    newCell2 =
                                        { pxl | tax = tax.amount }
                                in
                                { m2
                                    | selectCell =
                                        LoadedCell ( newCell2, s, e )
                                }

                            else
                                m2

                        _ ->
                            m
            in
            ( newModel, Cmd.none )

        RpcUbiEvent ubi ->
            let
                m =
                    { model
                        | acts = UbiAct ubi :: model.acts
                        , assets =
                            model.assets |> A.updateByUbi model.wallet ubi
                    }
            in
            ( case model.selectCell of
                LoadedCell ( { index } as pxl, s, e ) ->
                    if index == ubi.index then
                        let
                            newCell =
                                { pxl | ubi = zeroPrice }
                        in
                        { m
                            | selectCell = LoadedCell ( newCell, s, e )
                        }

                    else
                        m

                _ ->
                    m
            , Cmd.none
            )

        RpcRegistryTransferEvent rt ->
            let
                m =
                    { model | acts = DefaultAct rt :: model.acts }
            in
            ( case model.selectCell of
                LoadedCell ( { index } as pxl, _, e ) ->
                    if rt.index == index then
                        let
                            cell =
                                { pxl
                                    | owner = rt.to
                                    , price =
                                        m.taxInfo.mintTax
                                            |> Maybe.withDefault pxl.price
                                }
                        in
                        { m
                            | selectCell =
                                LoadedCell ( cell, PickNewColor, e )
                        }

                    else
                        m

                _ ->
                    m
            , Cmd.none
            )

        RpcTokenInfo { kind, address, amount } ->
            let
                updateWallet w m =
                    if Just address == w.address then
                        let
                            newWallet =
                                case kind of
                                    AllowanceTokenInfo ->
                                        Wallet { w | allowance = Just amount }

                                    BalanceTokenInfo ->
                                        Wallet { w | balance = Just amount }
                        in
                        { m | wallet = newWallet }

                    else
                        m

                updateSelectCell pxl s e m =
                    if address == pxl.owner then
                        let
                            newCell =
                                case kind of
                                    AllowanceTokenInfo ->
                                        { pxl | ownerAllowance = Just amount }

                                    BalanceTokenInfo ->
                                        { pxl | ownerBalance = Just amount }
                        in
                        { m | selectCell = LoadedCell ( newCell, s, e ) }

                    else
                        m

                newModel =
                    case ( model.selectCell, model.wallet ) of
                        ( LoadedCell ( pxl, s, e ), Wallet w ) ->
                            model
                                |> updateSelectCell pxl s e
                                |> updateWallet w

                        ( LoadedCell ( pxl, s, e ), _ ) ->
                            model |> updateSelectCell pxl s e

                        ( _, Wallet w ) ->
                            model |> updateWallet w

                        _ ->
                            model
            in
            ( newModel, Cmd.none )

        RpcPixel pixel ->
            let
                m =
                    { model | assets = model.assets |> A.updateByPixel pixel }
            in
            case model.selectCell of
                LoadingCell index ->
                    if index == pixel.index then
                        let
                            input =
                                m.input

                            newInput =
                                { input | newPrice = pixel.price }

                            newSelectCell =
                                LoadedCell ( pixel, PickNewColor, Nothing )

                            newModel =
                                { m
                                    | input = newInput
                                    , selectCell = newSelectCell
                                }
                        in
                        ( newModel, Rpc.getTokenInfo pixel.owner )

                    else
                        ( m, Cmd.none )

                _ ->
                    ( m, Cmd.none )

        RpcAssets page ->
            handleAssets model <| A.addPage page

        RpcDeltaCids cids ->
            handlePlayback model <| PB.initDeltaCids cids

        RpcError err ->
            ( { model | acts = ActError err :: model.acts }, Cmd.none )



-- Subs


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <| browserSubs ++ Rpc.rpcSubs ++ C.canvasSubs model


browserSubs : List (Sub Msg)
browserSubs =
    [ E.onMouseMove <| mouseEventDecoder MouseMove
    , E.onMouseUp <| mouseEventDecoder MouseUp
    , E.onResize (\w h -> WindowResize ( w, h ))
    ]


mouseEventDecoder : (Position -> Msg) -> Decoder Msg
mouseEventDecoder msg =
    D.map2
        (\x y -> msg <| position x y)
        (D.field "pageX" D.float)
        (D.field "pageY" D.float)
