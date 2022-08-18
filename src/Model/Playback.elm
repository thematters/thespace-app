module Model.Playback exposing
    ( Action(..)
    , ColorChange
    , DeltaData
    , Handler
    , Playback
    , Speed(..)
    , Timeline
    , addColorEvent
    , addColorEvents
    , addDeltaData
    , currentProgress
    , currentSpeed
    , deltaDataDecoder
    , enter
    , exit
    , init
    , initDeltaCids
    , initEmpty
    , jumpTo
    , maxProgress
    , pause
    , play
    , playing
    , readyToEnter
    , readyToPlay
    , setRewindTimeline
    , setSnapshotReady
    , speedToString
    , speedUp
    , tick
    )

import Array exposing (Array)
import Config exposing (genesisSnapshotCid, playbackWindow)
import Contract.Snapper exposing (Cid)
import Contract.Space exposing (ColorEvent)
import Data
    exposing
        ( BlockNumber
        , ColorId
        , Index
        , dec
        , inc
        , safeColorId
        , sum
        )
import Dict exposing (Dict)
import Http
import Json.Decode as D



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
    , snapshotReady : Bool
    , timelineReady : Bool
    , speed : Speed
    , snapshot : Cid
    , timeline : ForwardTimeline
    , rewindTimeline : RewindTimeline
    , events : Events
    }


type alias LoadingDataDeltas =
    Dict Cid DeltaData


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


type alias DeltaData =
    { delta : ColorChangeDelta
    , prev : Maybe Cid
    , snapshot : Cid
    , cid : Cid
    }


type alias ColorChangeDelta =
    List ColorChangeDeltaBlock


type alias ColorChangeDeltaBlock =
    { blockNumber : BlockNumber
    , changes : List ColorChangeBare
    }


type alias ColorChangeBare =
    { index : Index
    , color : ColorId
    }


type alias ColorChangeWithMeta =
    { blockNumber : BlockNumber
    , index : Index
    , color : ColorId
    }


type alias ColorChange cc =
    { cc | index : Int, color : Int }


type alias Timeline t =
    Array (ColorChange t)


type alias ForwardTimeline =
    Array ColorChangeWithMeta


type alias RewindTimeline =
    Array ColorChangeBare


type Action
    = LoadDeltas (List Cid)
    | InitSnapshot Cid
    | BuildRewindTimeline ForwardTimeline
    | EnterPlayback
    | Forward ForwardTimeline
    | Rewind RewindTimeline
    | SetSpeed Speed
    | PlayAgain
    | ExitPlayback
    | NoAction


type alias LoadedDataBlocks =
    Dict BlockNumber ColorChangeDeltaBlock


type alias Handler =
    Playback -> ( Playback, Action )



-- API


init : Playback
init =
    Loading
        { cids = []
        , deltas = Dict.empty
        , events = Array.empty
        }


initEmpty : Handler
initEmpty _ =
    ( Ready
        { status = Paused 0
        , snapshotReady = False
        , timelineReady = False
        , speed = OneX
        , snapshot = genesisSnapshotCid
        , timeline = Array.empty
        , rewindTimeline = Array.empty
        , events = Array.empty
        }
    , InitSnapshot genesisSnapshotCid
    )


initDeltaCids : List Cid -> Handler
initDeltaCids cids pb =
    case pb of
        Loading loadingData ->
            ( Loading { loadingData | cids = cids }, LoadDeltas cids )

        Ready _ ->
            ( pb, NoAction )


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


addDeltaData : Result Http.Error DeltaData -> Handler
addDeltaData jsonData pb =
    case jsonData of
        Err _ ->
            ( pb, NoAction )

        Ok data ->
            addDeltaData_ data pb


addDeltaData_ : DeltaData -> Handler
addDeltaData_ deltaData pb =
    case pb of
        Ready _ ->
            ( pb, NoAction )

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
                        , LoadDeltas [ nextCid ]
                        )

                    else
                        ( Loading newData, NoAction )

                _ ->
                    let
                        readyData =
                            finishLoading newData
                    in
                    ( Ready readyData
                    , InitSnapshot readyData.snapshot
                    )


setSnapshotReady : Handler
setSnapshotReady pb =
    ( case pb of
        Ready data ->
            Ready { data | snapshotReady = True }

        _ ->
            pb
    , NoAction
    )


setRewindTimeline : List String -> Handler
setRewindTimeline colorIds pb =
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
                cs
                (Array.toList tl)
                |> Array.fromList
    in
    case pb of
        Ready ({ timeline } as data) ->
            ( Ready
                { data
                    | timelineReady = True
                    , rewindTimeline = revTimeline timeline colorIds
                }
            , EnterPlayback
            )

        _ ->
            ( pb, NoAction )


readyToEnter : Playback -> Bool
readyToEnter pb =
    case pb of
        Ready { snapshotReady, timeline } ->
            snapshotReady && Array.length timeline >= 0

        _ ->
            False


readyToPlay : Playback -> Bool
readyToPlay pb =
    case pb of
        Ready { timelineReady } ->
            timelineReady

        _ ->
            False


playing : Playback -> Bool
playing pb =
    case pb of
        Ready { status } ->
            case status of
                Playing _ ->
                    True

                _ ->
                    False

        _ ->
            False


currentProgress : Playback -> Progress
currentProgress pb =
    case pb of
        Ready { status } ->
            case status of
                Playing i ->
                    i

                Paused i ->
                    i

        _ ->
            0


maxProgress : Playback -> Progress
maxProgress pb =
    case pb of
        Ready { timeline } ->
            limit timeline

        _ ->
            0


currentSpeed : Playback -> Speed
currentSpeed pb =
    case pb of
        Ready { speed } ->
            speed

        _ ->
            OneX


enter : Handler
enter pb =
    case pb of
        Ready data ->
            let
                newData =
                    if needRefresh data then
                        data |> refreshLoaded

                    else
                        data
            in
            ( Ready { newData | status = Paused 0 }
            , BuildRewindTimeline newData.timeline
            )

        _ ->
            ( pb, NoAction )


play : Handler
play pb =
    case pb of
        Ready ({ status, timeline } as data) ->
            case status of
                Paused i ->
                    if i == limit timeline then
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


pause : Handler
pause pb =
    ( case pb of
        Ready ({ status } as data) ->
            case status of
                Playing i ->
                    Ready { data | status = Paused i }

                _ ->
                    pb

        _ ->
            pb
    , NoAction
    )


tick : Handler
tick pb =
    case pb of
        Ready ({ status, timeline } as data) ->
            case status of
                Playing i ->
                    if i == limit timeline then
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


speedUp : Handler
speedUp pb =
    case pb of
        Ready ({ speed } as data) ->
            let
                newSpeed =
                    speed |> nextSpeed
            in
            ( Ready { data | speed = newSpeed }, SetSpeed newSpeed )

        _ ->
            ( pb, NoAction )


jumpTo : Int -> Handler
jumpTo i pb =
    case pb of
        Ready ({ status, timeline, rewindTimeline } as data) ->
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
                    Forward <| Array.slice curr i timeline

                EQ ->
                    NoAction

                LT ->
                    Rewind <| Array.slice i curr rewindTimeline
            )

        _ ->
            ( pb, NoAction )


exit : Handler
exit pb =
    case pb of
        Loading _ ->
            ( pb, NoAction )

        Ready data ->
            ( Ready { data | speed = OneX }, ExitPlayback )



-- Helpers


stepLength : Int
stepLength =
    100


limit : ForwardTimeline -> Progress
limit =
    --Array.length >> dec
    Array.length


safeProgress : ForwardTimeline -> Progress -> Progress
safeProgress =
    limit >> clamp 0


finishLoading : LoadingData -> LoadedData
finishLoading { cids, deltas, events } =
    { status = Paused 0
    , snapshotReady = False
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
    , rewindTimeline = Array.empty
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


loadingDataToTimeline : LoadingDataDeltas -> Events -> ForwardTimeline
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


blocksToTimeline : LoadedDataBlocks -> ForwardTimeline
blocksToTimeline blocks =
    blocks
        |> Dict.values
        |> List.sortBy .blockNumber
        |> List.map toInfoColorChangeList
        |> List.foldr (++) []
        |> Array.fromList


updateTimeline : Events -> ForwardTimeline -> ForwardTimeline
updateTimeline events oldTimeline =
    events |> Array.map eventToInfoColorChange |> Array.append oldTimeline


toInfoColorChange : BlockNumber -> ColorChangeBare -> ColorChangeWithMeta
toInfoColorChange blockNumber { index, color } =
    { blockNumber = blockNumber, index = index, color = color }


toInfoColorChangeList : ColorChangeDeltaBlock -> List ColorChangeWithMeta
toInfoColorChangeList { blockNumber, changes } =
    changes |> List.map (toInfoColorChange blockNumber)


deltaDataLength : DeltaData -> Int
deltaDataLength =
    .delta >> deltaLength


deltaLength : ColorChangeDelta -> Int
deltaLength =
    List.map (.changes >> List.length) >> sum


eventToInfoColorChange : ColorEvent -> ColorChangeWithMeta
eventToInfoColorChange { blockNumber, index, color } =
    { blockNumber = blockNumber, index = index, color = color }


stepForward : Int -> ForwardTimeline -> ( Int, Action )
stepForward i timeline =
    let
        timelineLen =
            Array.length timeline

        stepLen =
            (timelineLen // 120) |> max 1 |> min stepLength

        newProgress =
            i + stepLen |> safeProgress timeline

        cs =
            Array.slice i newProgress timeline
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



-- Decoders


deltaDataDecoder : Cid -> D.Decoder DeltaData
deltaDataDecoder cid =
    D.map4 DeltaData
        (D.at [ "delta" ] (D.list deltaBlockDecoder))
        (D.at [ "prev" ] (D.maybe D.string))
        (D.at [ "snapshot" ] D.string)
        (D.succeed cid)


deltaBlockDecoder : D.Decoder ColorChangeDeltaBlock
deltaBlockDecoder =
    D.map2
        ColorChangeDeltaBlock
        --    (D.at [ "time" ] Iso8601.decoder)
        (D.at [ "bk" ] D.int)
        (D.at [ "cs" ] (D.list colorChangeDecoder))


colorChangeDecoder : D.Decoder ColorChangeBare
colorChangeDecoder =
    D.map2 ColorChangeBare
        (D.at [ "i" ] D.int)
        (D.at [ "c" ] D.int)
