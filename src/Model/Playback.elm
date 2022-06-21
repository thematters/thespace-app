module Model.Playback exposing
    ( ColorChange
    , ColorChangeDeltaBlock
    , ColorChangeDeltaData
    , LoadingAction(..)
    , Playback(..)
    , Speed(..)
    , addColorEvent
    , addColorEvents
    , addDeltaData
    , init
    , initDeltaCids
    )

import Array exposing (Array)
import Config
import Data exposing (BlockNumber, Cid, ColorEvent, ColorId, Index)
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
    , blocks : LoadedDataBlocks
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
                loadedLength ds =
                    ds |> Dict.values |> List.map deltaDataLength |> sum

                aboveWindow x =
                    x >= Config.playbackWindow

                genesisCid =
                    "genesis"

                newLoadedDeltas =
                    let
                        cid =
                            case deltaData.prev of
                                Nothing ->
                                    -- first Delta
                                    genesisCid

                                Just cid_ ->
                                    -- user prev for `this` Delta
                                    cid_
                    in
                    deltas |> Dict.insert cid deltaData

                allCidLoaded =
                    let
                        notLoaded x =
                            Dict.member x newLoadedDeltas |> not
                    in
                    cids |> List.filter notLoaded |> List.length |> (==) 1

                noMoreToLoad =
                    if allCidLoaded then
                        Dict.member genesisCid deltas
                            || (loadedLength newLoadedDeltas |> aboveWindow)

                    else
                        False

                newLoadingData =
                    { loadingData | deltas = newLoadedDeltas }
            in
            case ( noMoreToLoad, deltaData.prev ) of
                ( False, Just nextCid ) ->
                    ( Loading { newLoadingData | cids = nextCid :: cids }
                    , LoadMore nextCid
                    )

                _ ->
                    ( Ready <| finishLoading <| newLoadingData
                    , None
                    )


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
            Ready { newData | status = Playing 0 }

        _ ->
            pb


pause : Playback -> Playback
pause pb =
    case pb of
        Ready data ->
            case data.status of
                Playing i ->
                    Ready { data | status = Paused i }

                _ ->
                    pb

        _ ->
            pb


speedUp : Playback -> Playback
speedUp pb =
    case pb of
        Ready data ->
            Ready { data | speed = data.speed |> nextSpeed }

        _ ->
            pb


jumpTo : Int -> Playback -> Playback
jumpTo i pb =
    case pb of
        Ready data ->
            let
                prog =
                    i |> clamp 0 (Array.length data.timeline - 1)
            in
            case data.status of
                Playing _ ->
                    Ready { data | status = Playing prog }

                Paused _ ->
                    Ready { data | status = Paused prog }

        _ ->
            pb


skipToStart : Playback -> Playback
skipToStart pb =
    case pb of
        Ready data ->
            case data.status of
                Playing i ->
                    Ready { data | status = Paused 0 }

                _ ->
                    pb

        _ ->
            pb


skipToEnd : Playback -> Playback
skipToEnd pb =
    case pb of
        Ready data ->
            case data.status of
                Playing i ->
                    Ready
                        { data
                            | status = Paused <| Array.length data.timeline - 1
                        }

                _ ->
                    pb

        _ ->
            pb



-- Helpers


finishLoading : LoadingData -> LoadedData
finishLoading loadingData =
    let
        blocks =
            blocksFromLoadingData loadingData
    in
    { status = Paused 0
    , speed = OneX
    , blocks = blocks
    , timeline = blocksToTimeline blocks
    , reverseTimeline = Array.empty
    , events = []
    }


needRefresh : LoadedData -> Bool
needRefresh =
    .events >> List.length >> (>) 0


refreshLoaded : LoadedData -> LoadedData
refreshLoaded { blocks, events } =
    let
        newBlocks =
            accColorEvents events blocks
    in
    { status = Paused 0
    , speed = OneX
    , blocks = newBlocks
    , timeline = blocksToTimeline newBlocks
    , reverseTimeline = Array.empty
    , events = []
    }


blocksFromLoadingData : LoadingData -> LoadedDataBlocks
blocksFromLoadingData { deltas, events } =
    let
        blockList =
            deltas |> Dict.values |> List.map .delta |> List.foldl (++) []

        bkNums =
            blockList |> List.map .blockNumber

        blocks =
            Dict.fromList <| List.map2 (\bk blk -> ( bk, blk )) bkNums blockList
    in
    accColorEvents events blocks


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


blocksToTimeline : LoadedDataBlocks -> Timeline
blocksToTimeline blocks =
    blocks
        |> Dict.values
        |> List.sortBy .blockNumber
        |> List.map toInfoColorChangeList
        |> List.foldl (++) []
        |> Array.fromList


toInfoColorChange : BlockNumber -> ColorChange -> BlockInfoColorChange
toInfoColorChange bk { index, color } =
    { index = index, color = color, blockNumber = bk }


toInfoColorChangeList : ColorChangeDeltaBlock -> List BlockInfoColorChange
toInfoColorChangeList { blockNumber, changes } =
    changes |> List.map (toInfoColorChange blockNumber)


deltaDataLength : ColorChangeDeltaData -> Int
deltaDataLength =
    .delta >> deltaLength


deltaLength : ColorChangeDelta -> Int
deltaLength =
    List.map (.changes >> List.length) >> sum


sum : List Int -> Int
sum =
    List.foldl (+) 0


colorEventToBlockInfoColorChange : ColorEvent -> BlockInfoColorChange
colorEventToBlockInfoColorChange { blockNumber, index, color } =
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
