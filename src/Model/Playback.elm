module Model.Playback exposing
    ( BlockInfoColorChange
    , ColorChange
    , ColorChangeDeltaBlock
    , ColorChangeDeltaData
    , LoadingAction(..)
    , Playback(..)
    , Speed(..)
    , Status(..)
    , Timeline
    , addColorEvent
    , addColorEvents
    , addDeltaData
    , getStep
    , init
    , initDeltaCids
    , maxProgress
    , pause
    , play
    , speedToString
    , speedUp
    , start
    , step
    )

import Array exposing (Array)
import Config exposing (genesisSnapshotCid, playbackWindow)
import Data exposing (BlockNumber, Cid, ColorEvent, ColorId, Index, dec, sum)
import Dict exposing (Dict)
import Time exposing (Posix)



-- Types


type Playback
    = Loading LoadingData
    | Ready LoadedData


type alias LoadingData =
    { cids : List Cid
    , deltas : LoadingDataDeltas
    , events : List ColorEvent
    }


type alias LoadingDataDeltas =
    Dict Cid ColorChangeDeltaData


type alias LoadedData =
    { status : Status
    , speed : Speed
    , snapshot : Cid
    , timeline : Timeline
    , reverseTimeline : Timeline
    , events : List ColorEvent
    }


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
    | None



-- API


init : Playback
init =
    Loading
        { cids = []
        , deltas = Dict.empty
        , events = []
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
            { data | events = c :: data.events }
    in
    case pb of
        Loading loadingData ->
            Loading <| addOne cevt loadingData

        Ready loadedData ->
            Ready <| addOne cevt loadedData


addColorEvents : List ColorEvent -> Playback -> Playback
addColorEvents cevts pb =
    let
        addMany cs data =
            { data | events = data.events ++ List.reverse cs }
    in
    case pb of
        Loading loadingData ->
            Loading <| addMany cevts loadingData

        Ready loadedData ->
            Ready <| addMany cevts loadedData


addDeltaData : ColorChangeDeltaData -> Playback -> ( Playback, LoadingAction )
addDeltaData deltaData pb =
    case pb of
        Ready _ ->
            ( pb, None )

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
                        |> (<) playbackWindow

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
                        ( Loading newData, None )

                _ ->
                    ( Ready <| finishLoading newData, None )


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


play : Playback -> Playback
play pb =
    case pb of
        Ready ({ status } as data) ->
            case status of
                Paused i ->
                    Ready { data | status = Playing i }

                _ ->
                    pb

        _ ->
            pb


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


step : Playback -> Playback
step pb =
    case pb of
        Ready ({ status, timeline } as data) ->
            case status of
                Playing i ->
                    let
                        stepLen =
                            Array.length timeline // 120
                    in
                    Ready { data | status = Playing <| i + stepLen }

                Paused _ ->
                    pb

        _ ->
            pb


getStep : Playback -> List BlockInfoColorChange
getStep pb =
    case pb of
        Ready ({ status, timeline } as data) ->
            case status of
                Playing i ->
                    let
                        stepLen =
                            Array.length timeline // 120
                    in
                    Array.slice i (i + stepLen) timeline |> Array.toList

                Paused _ ->
                    []

        _ ->
            []


speedUp : Playback -> Playback
speedUp pb =
    case pb of
        Ready ({ speed } as data) ->
            Ready { data | speed = speed |> nextSpeed }

        _ ->
            pb


jumpTo : Int -> Playback -> Playback
jumpTo i pb =
    case pb of
        Ready ({ status, timeline } as data) ->
            let
                prog =
                    i |> safeProgress timeline
            in
            case status of
                Playing _ ->
                    Ready { data | status = Playing prog }

                Paused _ ->
                    Ready { data | status = Paused prog }

        _ ->
            pb


skipToStart : Playback -> Playback
skipToStart pb =
    case pb of
        Ready ({ status } as data) ->
            case status of
                Playing i ->
                    Ready { data | status = Paused 0 }

                _ ->
                    pb

        _ ->
            pb


skipToEnd : Playback -> Playback
skipToEnd pb =
    case pb of
        Ready ({ status, timeline } as data) ->
            case status of
                Playing i ->
                    Ready { data | status = Paused <| maxProgress timeline }

                _ ->
                    pb

        _ ->
            pb


maxProgress : Timeline -> Int
maxProgress =
    Array.length >> dec


safeProgress : Timeline -> Int -> Int
safeProgress =
    maxProgress >> clamp 0



-- Helpers


finishLoading : LoadingData -> LoadedData
finishLoading { cids, deltas, events } =
    { status = Paused 0
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
    , events = []
    }


needRefresh : LoadedData -> Bool
needRefresh =
    .events >> List.length >> (>) 0


refreshLoaded : LoadedData -> LoadedData
refreshLoaded ({ timeline, events } as oldData) =
    { oldData
        | status = Paused 0
        , speed = OneX
        , timeline = updateTimeline events timeline
        , events = []
    }


accColorEvents : List ColorEvent -> LoadedDataBlocks -> LoadedDataBlocks
accColorEvents events blocks =
    events |> List.foldr accColorEvent blocks


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


loadingDataToTimeline : LoadingDataDeltas -> List ColorEvent -> Timeline
loadingDataToTimeline deltas events =
    blocksFromLoadingData deltas events |> blocksToTimeline


blocksFromLoadingData : LoadingDataDeltas -> List ColorEvent -> LoadedDataBlocks
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
        |> List.foldl (++) []
        |> Array.fromList


updateTimeline : List ColorEvent -> Timeline -> Timeline
updateTimeline events oldTimeline =
    events
        |> List.reverse
        |> List.map eventToInfoColorChange
        |> Array.fromList
        |> Array.append oldTimeline


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


speedToString : Speed -> String
speedToString spd =
    case spd of
        OneX ->
            "1X"

        TwoX ->
            "2X"

        FourX ->
            "4X"


nextSpeed : Speed -> Speed
nextSpeed spd =
    case spd of
        OneX ->
            TwoX

        TwoX ->
            FourX

        FourX ->
            OneX
