module Model.Playback exposing
    ( LoadingAction(..)
    , Playback
    , Speed(..)
    , addColorChange
    , addDeltaData
    , init
    , initDeltaCids
    )

import Array exposing (Array)
import Config
import Data
    exposing
        ( BlockNumber
        , Cid
        , ColorChangeDelta
        , ColorChangeDeltaBlock
        , ColorChangeDeltaData
        , ColorEvent
        , ColorId
        , Index
        )
import Dict exposing (Dict)
import Time exposing (Posix)



-- Types


type Playback
    = Loading LoadingData
    | Ready LoadedData


type alias LoadingData =
    { cids : List Cid
    , deltas : Dict Cid ColorChangeDeltaData
    , events : List ColorEvent
    }


type alias LoadedData =
    { status : Status
    , speed : Speed
    , blocks : Dict BlockNumber ColorChangeDeltaBlock
    , timeline : Array BlockInfoColorChange
    , reverseTimeline : Array BlockInfoColorChange
    , events : List ColorEvent
    }


type Status
    = Playing Progress
    | Paused


type alias Progress =
    Int


type Speed
    = OneX
    | TwoX
    | FourX


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
initDeltaCids cids pb_ =
    case pb_ of
        Loading pb ->
            Loading { pb | cids = cids }

        Ready _ ->
            pb_


addColorChange : ColorEvent -> Playback -> Playback
addColorChange cevt pb =
    let
        addOne c data =
            { data | events = c :: data.events }
    in
    case pb of
        Loading loadingData ->
            Loading <| addOne cevt loadingData

        Ready loadedData ->
            Ready <| addOne cevt loadedData


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

                allCidLoaded =
                    let
                        notLoaded x =
                            Dict.member x deltas |> not
                    in
                    cids |> List.filter notLoaded |> List.isEmpty

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

                noMoreToLoad =
                    if allCidLoaded then
                        Dict.member genesisCid deltas
                            || (loadedLength newLoadedDeltas |> aboveWindow)

                    else
                        False

                newLoadingData =
                    { loadingData | deltas = newLoadedDeltas }
            in
            case ( noMoreToLoad, List.head cids ) of
                ( False, Just nextCid ) ->
                    ( Loading { newLoadingData | cids = nextCid :: cids }
                    , LoadMore nextCid
                    )

                _ ->
                    ( Ready <| finishLoading <| newLoadingData
                    , None
                    )


finishLoading : LoadingData -> LoadedData
finishLoading loadingData =
    let
        blocks =
            loadingData.deltas |> deltasToBlocks
    in
    { status = Paused
    , speed = OneX
    , blocks = blocks
    , timeline = timeline blocks loadingData.events
    , reverseTimeline = Array.empty
    , events = []
    }


deltasToBlocks _ =
    Dict.empty


timeline _ _ =
    Array.empty



-- Helpers


sum : List Int -> Int
sum =
    List.foldl (+) 0


deltaLength : ColorChangeDelta -> Int
deltaLength =
    List.map (.changes >> List.length) >> sum


deltaDataLength : ColorChangeDeltaData -> Int
deltaDataLength =
    .delta >> deltaLength


colorEventToBlockInfoColorChange : ColorEvent -> BlockInfoColorChange
colorEventToBlockInfoColorChange { blockNumber, index, color } =
    { blockNumber = blockNumber, index = index, color = color }



-- Speed
--type alias PlaybackConfig =
--    { from : Int
--    , to : Int
--    , shareFrom : Int
--    , shareTo : Int
--    , current : Int
--    , status : PlaybackStatus
--    , speed : PlaybackSpeed
--    }
--type PlaybackStatus
--    = Playing
--    | Paused
--initPlaybackConfig : BlockNumber -> BlockNumber -> PlaybackConfig
--initPlaybackConfig from to =
--    { from = from
--    , to = to
--    , shareFrom = from
--    , shareTo = to
--    , speed = OneX
--    , current = 0
--    , status = Paused
--    }


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
