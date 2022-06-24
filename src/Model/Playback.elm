module Model.Playback exposing
    ( BlockInfoColorChange
    , ColorChange
    , ColorChangeDeltaBlock
    , ColorChangeDeltaData
    , LoadingAction(..)
    , PlayAction(..)
    , Playback(..)
    , Speed(..)
    , Status(..)
    , Timeline
    , addColorEvent
    , addColorEvents
    , addDeltaData
    , init
    , initDeltaCids
    , initPlayback
    , jumpTo
    , maxProgress
    , pause
    , play
    , readyToSlide
    , readyToStart
    , setCanvasReady
    , setReverseTimeline
    , speedToString
    , speedUp
    , start
    , tick
    )

import Array exposing (Array)
import Config exposing (genesisSnapshotCid, playbackWindow)
import Data
    exposing
        ( BlockNumber
        , Cid
        , ColorEvent
        , ColorId
        , Index
        , dec
        , inc
        , safeColorId
        , sum
        )
import Dict exposing (Dict)
import Time exposing (Posix)



-- Types


type Playback
    = Loading LoadingData
    | Ready LoadedData


type alias LoadingData =
    { cids : List Cid
    , deltas : LoadingDataDeltas
    , events : Events
    }


type alias LoadedData =
    { status : Status
    , canvasReady : Bool
    , timelineReady : Bool
    , speed : Speed
    , snapshot : Cid
    , timeline : Timeline
    , reverseTimeline : Array ColorChange
    , events : Events
    }


type alias LoadingDataDeltas =
    Dict Cid ColorChangeDeltaData


type alias Events =
    Array ColorEvent


type Status
    = Playing Progress
    | Paused Progress


type alias Progress =
    Int


type Speed
    = OneX
    | TwoX
    | FourX


type alias LoadedDataBlocks =
    Dict BlockNumber ColorChangeDeltaBlock


type alias ColorChangeDeltaData =
    { delta : ColorChangeDelta
    , prev : Maybe Cid
    , snapshot : Cid
    , cid : Cid
    }


type alias ColorChangeDelta =
    List ColorChangeDeltaBlock


type alias ColorChangeDeltaBlock =
    { blockNumber : BlockNumber
    , changes : List ColorChange
    }


type alias ColorChange =
    { index : Index
    , color : ColorId
    }


type alias Timeline =
    Array BlockInfoColorChange


type alias BlockInfoColorChange =
    { blockNumber : BlockNumber
    , index : Index
    , color : ColorId
    }


type LoadingAction
    = LoadMore Cid
    | Standby


type PlayAction
    = PlayAgain
    | Forward (List BlockInfoColorChange)
    | Rewind (List ColorChange)
    | NoAction



-- API


init : Playback
init =
    Loading
        { cids = []
        , deltas = Dict.empty
        , events = Array.empty
        }


initPlayback =
    Loading
        { cids = []
        , deltas = Dict.empty
        , events = Array.empty
        }


initDeltaCids : List Cid -> Playback -> Playback
initDeltaCids cids pb =
    case pb of
        Loading loadingData ->
            Loading { loadingData | cids = cids }

        Ready _ ->
            pb


addColorEvent : ColorEvent -> Playback -> Playback
addColorEvent cevt pb =
    let
        addOne c data =
            { data | events = Array.push c data.events }
    in
    case pb of
        Loading loadingData ->
            Loading <| addOne cevt loadingData

        Ready loadedData ->
            Ready <| addOne cevt loadedData


addColorEvents : List ColorEvent -> Playback -> Playback
addColorEvents cevts pb =
    let
        cs =
            Array.fromList cevts

        addMany xs data =
            { data | events = Array.append data.events xs }
    in
    case pb of
        Loading loadingData ->
            Loading <| addMany cs loadingData

        Ready loadedData ->
            Ready <| addMany cs loadedData


addDeltaData : ColorChangeDeltaData -> Playback -> ( Playback, LoadingAction )
addDeltaData deltaData pb =
    case pb of
        Ready _ ->
            ( pb, Standby )

        Loading ({ cids, deltas } as loadingData) ->
            let
                allCidLoaded dts =
                    cids
                        |> List.filter (\x -> not <| Dict.member x dts)
                        |> List.isEmpty

                aboveWindow dts =
                    dts
                        |> Dict.values
                        |> List.map deltaDataLength
                        |> sum
                        |> (<=) playbackWindow

                newDeltas =
                    if List.member deltaData.cid cids then
                        deltas |> Dict.insert deltaData.cid deltaData

                    else
                        deltas

                newData =
                    { loadingData | deltas = newDeltas }
            in
            case
                ( allCidLoaded newDeltas && aboveWindow newDeltas
                , deltaData.prev
                )
            of
                ( False, Just nextCid ) ->
                    if not <| List.member nextCid cids then
                        ( Loading { newData | cids = nextCid :: cids }
                        , LoadMore nextCid
                        )

                    else
                        ( Loading newData, Standby )

                _ ->
                    ( Ready <| finishLoading newData, Standby )


setCanvasReady : Playback -> Playback
setCanvasReady pb =
    case pb of
        Ready data ->
            Ready { data | canvasReady = True }

        _ ->
            pb


setReverseTimeline : List String -> Playback -> Playback
setReverseTimeline colorIds pb =
    let
        revTimeline tl cs =
            List.map2
                (\c { index } ->
                    let
                        cId =
                            c
                                |> String.toInt
                                |> Maybe.withDefault 0
                                |> inc
                                |> safeColorId
                    in
                    { index = index, color = cId }
                )
                colorIds
                (Array.toList tl)
                |> Array.fromList
    in
    case pb of
        Ready ({ timeline } as data) ->
            Ready
                { data
                    | timelineReady = True
                    , reverseTimeline = revTimeline timeline colorIds
                }

        _ ->
            pb


readyToStart : Playback -> Bool
readyToStart pb =
    case pb of
        Ready { canvasReady, timeline } ->
            canvasReady && Array.length timeline > 0

        _ ->
            False


readyToSlide : Playback -> Bool
readyToSlide pb =
    case pb of
        Ready { timelineReady } ->
            timelineReady

        _ ->
            False


start : Playback -> Playback
start pb =
    case pb of
        Ready data ->
            let
                newData =
                    if needRefresh data then
                        data |> refreshLoaded

                    else
                        data
            in
            Ready { newData | status = Paused 0 }

        _ ->
            pb


play : Playback -> ( Playback, PlayAction )
play pb =
    case pb of
        Ready ({ status, timeline } as data) ->
            case status of
                Paused i ->
                    if i == maxProgress timeline then
                        ( Ready { data | status = Playing 0 }, PlayAgain )

                    else
                        let
                            ( newProgress, action ) =
                                stepForward i timeline
                        in
                        ( Ready { data | status = Playing newProgress }
                        , action
                        )

                _ ->
                    ( pb, NoAction )

        _ ->
            ( pb, NoAction )


pause : Playback -> Playback
pause pb =
    case pb of
        Ready ({ status } as data) ->
            case status of
                Playing i ->
                    Ready { data | status = Paused i }

                _ ->
                    pb

        _ ->
            pb


tick : Playback -> ( Playback, PlayAction )
tick pb =
    case pb of
        Ready ({ status, timeline } as data) ->
            case status of
                Playing i ->
                    if i == maxProgress timeline then
                        ( Ready { data | status = Paused i }
                        , NoAction
                        )

                    else
                        let
                            ( newProgress, action ) =
                                stepForward i timeline
                        in
                        ( Ready { data | status = Playing newProgress }
                        , action
                        )

                Paused _ ->
                    ( pb, NoAction )

        _ ->
            ( pb, NoAction )


speedUp : Playback -> Playback
speedUp pb =
    case pb of
        Ready ({ speed } as data) ->
            Ready { data | speed = speed |> nextSpeed }

        _ ->
            pb


jumpTo : Int -> Playback -> ( Playback, PlayAction )
jumpTo i pb =
    case pb of
        Ready ({ status, timeline, reverseTimeline } as data) ->
            let
                prog =
                    i |> safeProgress timeline

                ( curr, newPlayback ) =
                    case status of
                        Playing x ->
                            ( x, Ready { data | status = Paused prog } )

                        Paused x ->
                            ( x, Ready { data | status = Paused prog } )
            in
            ( newPlayback
            , case compare i curr of
                GT ->
                    Array.slice curr i timeline
                        |> Array.toList
                        |> Forward

                EQ ->
                    NoAction

                LT ->
                    Array.slice i curr reverseTimeline
                        |> Array.toList
                        |> Rewind
            )

        _ ->
            ( pb, NoAction )



-- Helpers


stepLenght : Int
stepLenght =
    120


maxProgress : Timeline -> Int
maxProgress =
    Array.length >> dec


safeProgress : Timeline -> Int -> Int
safeProgress =
    maxProgress >> clamp 0


finishLoading : LoadingData -> LoadedData
finishLoading { cids, deltas, events } =
    { status = Paused 0
    , canvasReady = False
    , timelineReady = False
    , speed = OneX
    , snapshot =
        cids
            |> List.head
            |> Maybe.map (\cid -> Dict.get cid deltas)
            |> Maybe.withDefault Nothing
            |> Maybe.map .snapshot
            |> Maybe.withDefault genesisSnapshotCid
    , timeline = loadingDataToTimeline deltas events
    , reverseTimeline = Array.empty
    , events = Array.empty
    }


needRefresh : LoadedData -> Bool
needRefresh =
    .events >> Array.length >> (<) 0


refreshLoaded : LoadedData -> LoadedData
refreshLoaded ({ timeline, events } as oldData) =
    { oldData
        | status = Paused 0
        , speed = OneX
        , timeline = updateTimeline events timeline
        , events = Array.empty
    }


accColorEvents : Events -> LoadedDataBlocks -> LoadedDataBlocks
accColorEvents events blocks =
    events |> Array.foldr accColorEvent blocks


accColorEvent : ColorEvent -> LoadedDataBlocks -> LoadedDataBlocks
accColorEvent { blockNumber, index, color } blocks =
    let
        update v =
            let
                c =
                    { index = index, color = color }
            in
            case v of
                Nothing ->
                    Just
                        { blockNumber = blockNumber, changes = [ c ] }

                Just cs ->
                    Just
                        { cs | changes = cs.changes ++ [ c ] }
    in
    blocks |> Dict.update blockNumber update


loadingDataToTimeline : LoadingDataDeltas -> Events -> Timeline
loadingDataToTimeline deltas events =
    blocksFromLoadingData deltas events |> blocksToTimeline


blocksFromLoadingData : LoadingDataDeltas -> Events -> LoadedDataBlocks
blocksFromLoadingData deltas events =
    let
        blockList =
            deltas |> Dict.values |> List.map .delta |> List.foldl (++) []

        bkNums =
            blockList |> List.map .blockNumber

        blocks =
            Dict.fromList <| List.map2 (\bk blk -> ( bk, blk )) bkNums blockList
    in
    accColorEvents events blocks


blocksToTimeline : LoadedDataBlocks -> Timeline
blocksToTimeline blocks =
    blocks
        |> Dict.values
        |> List.sortBy .blockNumber
        |> List.map toInfoColorChangeList
        |> List.foldr (++) []
        |> Array.fromList


updateTimeline : Events -> Timeline -> Timeline
updateTimeline events oldTimeline =
    events |> Array.map eventToInfoColorChange |> Array.append oldTimeline


toInfoColorChange : BlockNumber -> ColorChange -> BlockInfoColorChange
toInfoColorChange blockNumber { index, color } =
    { blockNumber = blockNumber, index = index, color = color }


toInfoColorChangeList : ColorChangeDeltaBlock -> List BlockInfoColorChange
toInfoColorChangeList { blockNumber, changes } =
    changes |> List.map (toInfoColorChange blockNumber)


deltaDataLength : ColorChangeDeltaData -> Int
deltaDataLength =
    .delta >> deltaLength


deltaLength : ColorChangeDelta -> Int
deltaLength =
    List.map (.changes >> List.length) >> sum


eventToInfoColorChange : ColorEvent -> BlockInfoColorChange
eventToInfoColorChange { blockNumber, index, color } =
    { blockNumber = blockNumber, index = index, color = color }


stepForward : Int -> Timeline -> ( Int, PlayAction )
stepForward i timeline =
    let
        historyLen =
            Array.length timeline

        stepLen =
            historyLen // stepLenght |> max stepLenght

        newProgress =
            i + stepLen |> safeProgress timeline

        cs =
            Array.slice i (i + stepLen) timeline
                |> Array.toList
    in
    ( newProgress, Forward cs )


nextSpeed : Speed -> Speed
nextSpeed spd =
    case spd of
        OneX ->
            TwoX

        TwoX ->
            FourX

        FourX ->
            OneX


speedToString : Speed -> String
speedToString spd =
    case spd of
        OneX ->
            "1X"

        TwoX ->
            "2X"

        FourX ->
            "4X"
