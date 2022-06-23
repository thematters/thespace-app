module Main exposing (main)

import Array
import Browser
import Browser.Events as E
import Canvas as C
import Config exposing (..)
import Data exposing (..)
import Dict
import Eth.Types exposing (Address)
import Html.Styled exposing (Html, toUnstyled)
import InfiniteList
import Json.Decode as D exposing (Decoder)
import Json.Encode exposing (Value)
import Model exposing (..)
import Model.Assets
    exposing
        ( Assets(..)
        , LoadedAssetsPage(..)
        , LoadedAssetsPages
        , LoadingAssets
        , assetsFinishLoading
        , initLoadingAssets
        , sortAssetsResult
        , updateAssetsByColor
        , updateAssetsByPixel
        , updateAssetsByPrice
        , updateAssetsByTransfer
        , updateAssetsByUbi
        , withinGetOwnPixelLimit
        )
import Model.Playback as PB
import Msg exposing (Msg(..))
import Rpc
import View exposing (view)



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }



-- Init


init : Flags -> ( Model, Cmd Msg )
init { winW, winH, centerCell, zoom } =
    let
        model =
            initModel

        canvas =
            model.canvas

        initZoom =
            case zoom of
                Nothing ->
                    minZoom

                Just z ->
                    z |> toFloat |> clamp minZoom maxZoom
    in
    ( { model
        | winSize = ( winW, winH )
        , selectCell =
            case centerCell of
                Nothing ->
                    NoCell

                Just cell ->
                    LoadingCell <| cellToIndex cell
        , canvas = { canvas | zoom = initZoom }
      }
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
                , assets = AssetsNotLoaded
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
                    , code = defaultRpcErrorCode -- we dont't care
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
                        , miniMapMode = responsiveMiniMapMode winSize
                        , sidebarMode =
                            responsiveSiebarMode
                                winSize
                                model.sidebarMode
                        , sidebarInfLists =
                            { actsInfList = InfiniteList.init
                            , assetsInfList = InfiniteList.init
                            }
                    }
                        |> resetModel
            in
            ( m, C.reset m.canvas m.winSize )

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
            ( m, C.transform m.canvas )

        MapMouseDown xy ->
            ( { model
                | dragging = MapDragging xy
                , pagePos = xy
                , cellPos = posToCell model.canvas xy
              }
            , Cmd.none
            )

        MapTouchDown xy ->
            ( if model.pinch == Nothing then
                { model
                    | dragging = MapDragging xy
                    , pagePos = xy
                    , cellPos = posToCell model.canvas xy
                }

              else
                model
            , Cmd.none
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
            , Cmd.batch [ C.transform cvs, Rpc.getPixel idx ]
            )

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

        ClearSelectedCell ->
            ( { model | selectCell = NoCell }, Cmd.none )

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
                ( uiMode, _ ) =
                    model.sidebarMode

                m =
                    { model | sidebarMode = ( uiMode, mode ) }
            in
            case ( mode, m.assets, m.wallet ) of
                ( AssetsManager, AssetsNotLoaded, Wallet { address } ) ->
                    requestOwnPixelsFirstPageOrNothing m address

                _ ->
                    ( m, Cmd.none )

        LoadAssets ->
            case ( model.sidebarMode, model.assets, model.wallet ) of
                ( ( _, AssetsManager ), AssetsNotLoaded, Wallet { address } ) ->
                    requestOwnPixelsFirstPageOrNothing model address

                _ ->
                    ( model, Cmd.none )

        RefreshAssets ->
            case ( model.sidebarMode, model.assets, model.wallet ) of
                ( ( _, AssetsManager ), AssetsLoaded _, Wallet { address } ) ->
                    requestOwnPixelsFirstPageOrNothing model address

                _ ->
                    ( model, Cmd.none )

        SortAssets sort ->
            case model.assets of
                AssetsLoaded assets ->
                    let
                        newList =
                            assets.list
                                |> sortAssetsResult sort assets.timeOrder
                    in
                    ( { model
                        | assets =
                            AssetsLoaded
                                { assets | list = newList, sort = sort }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

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
                            responsiveSiebarMode model.winSize model.sidebarMode
                        , miniMapMode = responsiveMiniMapMode model.winSize
                        , notif = Nothing
                      }
                    , Cmd.none
                    )

                Realtime ->
                    ( model, Cmd.none )

                PlaybackLoading ->
                    --( m, C.endPlayback pb )
                    ( { model
                        | mode = Realtime
                        , sidebarMode =
                            responsiveSiebarMode model.winSize model.sidebarMode
                        , miniMapMode = responsiveMiniMapMode model.winSize
                      }
                    , Cmd.none
                    )

                Playback ->
                    --( m, C.endPlayback pb )
                    ( { model
                        | mode = Realtime
                        , sidebarMode =
                            responsiveSiebarMode model.winSize model.sidebarMode
                        , miniMapMode = responsiveMiniMapMode model.winSize
                      }
                    , Cmd.none
                    )

        AppModeChange PlaybackLoading ->
            case model.mode of
                Realtime ->
                    case model.playback of
                        PB.Loading _ ->
                            ( model, Cmd.none )

                        PB.Ready { snapshot } ->
                            ( { model
                                | mode = PlaybackLoading
                                , miniMapMode = CollapsedMiniMap
                              }
                            , C.initPlayback <| cidToSnapshotUri snapshot
                            )

                _ ->
                    ( model, Cmd.none )

        AppModeChange Playback ->
            case model.mode of
                PlaybackLoading ->
                    ( { model
                        | mode = Playback
                        , playback = PB.start model.playback
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        -- Playback
        DeltaRecieved jsonData ->
            let
                ( newPlayback, action ) =
                    case jsonData of
                        Ok data ->
                            model.playback |> PB.addDeltaData data

                        Err _ ->
                            ( model.playback, PB.None )
            in
            ( { model | playback = newPlayback }
            , case action of
                PB.LoadMore cid ->
                    Rpc.getDeltas [ cid ]

                _ ->
                    Cmd.none
            )

        PlaybackStart ->
            --let
            --    handler config =
            --        if config.current == Array.length model.colorHistory then
            --            let
            --                newConfig =
            --                    { config
            --                        | current = 1
            --                        , status = Playing
            --                    }
            --            in
            --            ( { model | mode = Playback newConfig }
            --            , C.startPlaybackAgain
            --            )
            --        else
            --            let
            --                newConfig =
            --                    { config | status = Playing }
            --            in
            --            ( { model | mode = Playback newConfig }
            --            , C.startPlayback
            --            )
            --in
            --playbackHandlerHelper model handler
            --( model, Cmd.none )
            ( { model | playback = PB.play model.playback |> PB.step }
            , C.forward <| PB.getStep model.playback
            )

        PlaybackTick ->
            --let
            --    handler config =
            --        if config.status == Paused then
            --            ( model, Cmd.none )
            --        else
            --            let
            --                historyLength =
            --                    Array.length model.colorHistory
            --                step =
            --                    historyLength // playbackTicks |> max 1
            --                newCurrent =
            --                    config.current + step |> min historyLength
            --                newStatus =
            --                    if newCurrent == historyLength then
            --                        Paused
            --                    else
            --                        Playing
            --                newConfig =
            --                    { config
            --                        | current = newCurrent
            --                        , status = newStatus
            --                    }
            --                slice =
            --                    Array.slice
            --                        config.current
            --                        newCurrent
            --                        model.colorHistory
            --            in
            --            ( { model | mode = Playback newConfig }
            --            , C.forward slice
            --            )
            --in
            --playbackHandlerHelper model handler
            --( model, Cmd.none )
            ( { model | playback = PB.step model.playback }
            , C.forward <| PB.getStep model.playback
            )

        PlaybackSkipToStart ->
            --let
            --    handler config =
            --        let
            --            newConfig =
            --                { config
            --                    | current = 1
            --                    , status = Paused
            --                }
            --        in
            --        ( { model | mode = Playback newConfig }
            --        , C.playbackSkipToStart
            --        )
            --in
            --playbackHandlerHelper model handler
            ( model, Cmd.none )

        PlaybackSkipToEnd ->
            --let
            --    handler config =
            --        let
            --            newConfig =
            --                { config
            --                    | current = config.to
            --                    , status = Paused
            --                }
            --        in
            --        ( { model | mode = Playback newConfig }
            --        , C.playbackSkipToEnd
            --        )
            --in
            --playbackHandlerHelper model handler
            ( model, Cmd.none )

        PlaybackPause ->
            --let
            --    handler config =
            --        let
            --            newConfig =
            --                { config | status = Paused }
            --        in
            --        ( { model | mode = Playback newConfig }, Cmd.none )
            --in
            --playbackHandlerHelper model handler
            ( { model | playback = PB.pause model.playback }, Cmd.none )

        PlaybackCircleSpeed ->
            --let
            --    handler config =
            --        let
            --            newSpeed =
            --                nextSpeed config.speed
            --            newConfig =
            --                { config | speed = newSpeed }
            --        in
            --        ( { model | mode = Playback newConfig }
            --        , C.playbackChangeSpeed newSpeed
            --        )
            --in
            --playbackHandlerHelper model handler
            ( { model | playback = PB.speedUp model.playback }, Cmd.none )

        PlaybackSlide currentString ->
            --let
            --    handler config =
            --        case String.toInt currentString of
            --            Nothing ->
            --                ( model, Cmd.none )
            --            Just current ->
            --                let
            --                    newConfig =
            --                        { config
            --                            | current = current
            --                            , status = Paused
            --                        }
            --                    m =
            --                        { model | mode = Playback newConfig }
            --                in
            --                if current == config.current then
            --                    ( m, Cmd.none )
            --                else if current > config.current then
            --                    ( m
            --                    , C.forward <|
            --                        Array.slice
            --                            config.current
            --                            current
            --                            model.colorHistory
            --                    )
            --                else
            --                    ( m
            --                    , C.rewind <|
            --                        Array.slice
            --                            (current + 1)
            --                            (config.current + 1)
            --                            model.colorHistory
            --                    )
            --in
            --playbackHandlerHelper model handler
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- Handler Helpers


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



--type alias PlaybackHander msg =
--    PlaybackConfig -> ( Model, Cmd msg )
--playbackHandlerHelper : Model -> PlaybackHander msg -> ( Model, Cmd msg )
--playbackHandlerHelper model playbackHandler =
--    case model.mode of
--        RealtimeLoading ->
--            ( model, Cmd.none )
--        Realtime ->
--            ( model, Cmd.none )
--        PlaybackLoading ->
--            ( model, Cmd.none )
--        Playback config ->
--            playbackHandler config
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


handleSelectCell : Model -> Cell -> ( Model, Cmd msg )
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
    , Cmd.batch [ C.transform cvs, Rpc.getPixel index ]
    )


handleMapDragging : Model -> PositionDelta -> ( Model, Cmd msg )
handleMapDragging model { dx, dy } =
    let
        m =
            model |> removeSelectCell

        canvas =
            m.canvas |> C.moveTransform dx dy m.winSize
    in
    ( { m | canvas = canvas }, C.transform canvas )


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


requestOwnPixelsFirstPageOrNothing : Model -> Maybe Address -> ( Model, Cmd msg )
requestOwnPixelsFirstPageOrNothing m address =
    case address of
        Just addr ->
            ( { m | assets = initLoadingAssets m.blockNumber m.assets }
            , requestOwnPixelTestPage m.blockNumber addr
            )

        _ ->
            ( m, Cmd.none )



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
                , Rpc.getBlockNumber
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
                    case model.blockNumber of
                        Nothing ->
                            Just bkNum

                        Just bk ->
                            Just <| max bk bkNum

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
                    { model
                        | playback = model.playback |> PB.addColorEvents cs
                    }

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
                        , playback = model.playback |> PB.addColorEvent c
                    }

                m2 =
                    case model.selectCell of
                        LoadedCell ( { index } as pxl, _, e ) ->
                            if c.index == index then
                                let
                                    cell =
                                        { pxl | color = c.color }
                                in
                                { m
                                    | selectCell =
                                        LoadedCell ( cell, PickNewColor, e )
                                }

                            else
                                m

                        _ ->
                            m

                newModel =
                    case m2.assets of
                        AssetsLoaded assets ->
                            let
                                newAssets =
                                    updateAssetsByColor c assets
                            in
                            { m2 | assets = AssetsLoaded newAssets }

                        _ ->
                            m2
            in
            ( newModel, C.update [ c ] )

        RpcPriceEvent p ->
            let
                m =
                    { model
                        | acts = PriceAct p :: model.acts
                        , queue = model.queue |> Dict.remove p.index
                    }

                m2 =
                    case model.selectCell of
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

                newModel =
                    case m2.assets of
                        AssetsLoaded assets ->
                            let
                                newAssets =
                                    updateAssetsByPrice p assets
                            in
                            { m2 | assets = AssetsLoaded newAssets }

                        _ ->
                            m2
            in
            ( newModel, Cmd.none )

        RpcTransferEvent t ->
            let
                m =
                    { model
                        | acts = TransferAct t :: model.acts
                        , queue = model.queue |> Dict.remove t.index
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

                newModel =
                    case ( m2.wallet, m2.assets ) of
                        ( Wallet { address }, AssetsLoaded assets ) ->
                            let
                                newAssets =
                                    updateAssetsByTransfer address t assets
                            in
                            { m2 | assets = AssetsLoaded newAssets }

                        _ ->
                            m2

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
            ( newModel, cmd )

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
                    { model | acts = UbiAct ubi :: model.acts }

                m2 =
                    case model.selectCell of
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

                newModel =
                    case ( model.wallet, model.assets ) of
                        ( Wallet { address }, AssetsLoaded assets ) ->
                            if address == Just ubi.collector then
                                let
                                    newAssets =
                                        assets |> updateAssetsByUbi ubi
                                in
                                { m2 | assets = AssetsLoaded newAssets }

                            else
                                m2

                        _ ->
                            m2
            in
            ( newModel, Cmd.none )

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
                    case model.assets of
                        AssetsLoaded assets ->
                            let
                                newAssets =
                                    updateAssetsByPixel pixel assets
                            in
                            { model | assets = AssetsLoaded newAssets }

                        _ ->
                            model
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

        RpcOwnPixels page ->
            case ( model.wallet, model.assets ) of
                ( Wallet { address }, AssetsLoading loadingInfo ) ->
                    case address of
                        Just addr ->
                            handleGetOwnPixelPage model addr loadingInfo page

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RpcDeltaCids cids ->
            ( { model | playback = model.playback |> PB.initDeltaCids cids }
            , Rpc.getDeltas cids
            )

        RpcError err ->
            ( { model | acts = ActError err :: model.acts }, Cmd.none )



-- Rpc Handler Helpers


requestOwnPixelTestPage : Maybe BlockNumber -> Address -> Cmd msg
requestOwnPixelTestPage bk addr =
    Rpc.getOwnPixels bk addr getOwnPixelPage 0


handleGetOwnPixelPage : Model -> Address -> LoadingAssets -> OwnPixelsResultPage -> ( Model, Cmd msg )
handleGetOwnPixelPage model addr loadingInfo page =
    if justOnePage page then
        handleGetOwnPixelJustOnePage model loadingInfo page

    else if firstPage page then
        let
            newLoadingInfo =
                if withinGetOwnPixelLimit page.total then
                    addOnePage loadingInfo page

                else
                    loadingInfo

            newModel =
                if withinGetOwnPixelLimit page.total then
                    { model | assets = AssetsLoading newLoadingInfo }

                else
                    model
        in
        handleGetOwnPixeRequestAllPages newModel newLoadingInfo addr page

    else
        handleGetOwnPixeAddOnePage model loadingInfo page


handleGetOwnPixelJustOnePage : Model -> LoadingAssets -> OwnPixelsResultPage -> ( Model, Cmd msg )
handleGetOwnPixelJustOnePage model _ ({ pixels } as page) =
    ( { model | assets = assetsFinishLoading pixels page model.assets }
    , Cmd.none
    )


handleGetOwnPixeRequestAllPages : Model -> LoadingAssets -> Address -> OwnPixelsResultPage -> ( Model, Cmd msg )
handleGetOwnPixeRequestAllPages model { block, loaded, sort } addr page =
    let
        requestPage x =
            case x of
                Offset i ->
                    Rpc.getOwnPixels block addr getOwnPixelPage i

                _ ->
                    Cmd.none
    in
    case loaded of
        Nothing ->
            let
                offsets =
                    calculateOffsets page.total
            in
            ( { model
                | assets =
                    AssetsLoading
                        { block = block
                        , loaded = Just offsets
                        , sort = sort
                        }
              }
            , Cmd.batch <| List.map requestPage <| Array.toList offsets
            )

        Just loaded_ ->
            ( model, Cmd.batch <| List.map requestPage <| Array.toList loaded_ )


handleGetOwnPixeAddOnePage : Model -> LoadingAssets -> OwnPixelsResultPage -> ( Model, Cmd msg )
handleGetOwnPixeAddOnePage model info page =
    let
        newLoadingInfo =
            addOnePage info page

        m =
            { model | assets = AssetsLoading newLoadingInfo }
    in
    if allPageLoaded newLoadingInfo.loaded then
        case newLoadingInfo.loaded of
            Nothing ->
                ( m, Cmd.none )

            Just loaded ->
                let
                    aux a b =
                        case a of
                            Offset _ ->
                                b

                            Page p ->
                                b ++ p.pixels

                    pixels =
                        Array.foldl aux [] loaded
                in
                ( { m | assets = assetsFinishLoading pixels page model.assets }
                , Cmd.none
                )

    else
        ( m, Cmd.none )


allPageLoaded : LoadedAssetsPages -> Bool
allPageLoaded loaded_ =
    let
        isOffset x =
            case x of
                Offset _ ->
                    True

                _ ->
                    False
    in
    case loaded_ of
        Nothing ->
            False

        Just loaded ->
            loaded |> Array.filter isOffset |> Array.isEmpty


addOnePage : LoadingAssets -> OwnPixelsResultPage -> LoadingAssets
addOnePage loadingInfo page =
    let
        pageN =
            whichPage page

        page_ =
            Page <| { page | pixels = page.pixels }

        offsets =
            case loadingInfo.loaded of
                Nothing ->
                    calculateOffsets page.total

                Just loaded ->
                    loaded
    in
    { loadingInfo | loaded = Just <| Array.set pageN page_ offsets }


justOnePage : OwnPixelsResultPage -> Bool
justOnePage page =
    page.total <= page.limit


firstPage : OwnPixelsResultPage -> Bool
firstPage page =
    page.offset == 0


whichPage : OwnPixelsResultPage -> Int
whichPage page =
    let
        realOffset =
            if withinGetOwnPixelLimit page.total then
                page.offset

            else
                page.offset - page.total + getOwnPixelLimit
    in
    realOffset // getOwnPixelPage


calculateOffsets : Int -> Array.Array LoadedAssetsPage
calculateOffsets total =
    if total > getOwnPixelLimit then
        let
            pageNum =
                calculatePageNum getOwnPixelLimit getOwnPixelPage

            start =
                total - getOwnPixelLimit
        in
        Array.initialize pageNum (\i -> Offset <| i * getOwnPixelPage + start)

    else
        let
            pageNum =
                calculatePageNum total getOwnPixelPage
        in
        Array.initialize pageNum (\i -> Offset <| i * getOwnPixelPage)


calculatePageNum : Int -> Int -> Int
calculatePageNum totalNum pageLen =
    let
        pageNum =
            totalNum // pageLen
    in
    if pageNum * pageLen < totalNum then
        inc pageNum

    else
        pageNum



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



-- View


view : Model -> Html Msg
view =
    View.view
