module Model.Assets exposing
    ( Action(..)
    , Assets(..)
    , ChangeKind(..)
    , Changes
    , CollectedIds
    , FetchedPixels
    , Handler
    , LoadedData
    , LoadedPage(..)
    , Rank
    , RankType(..)
    , addPage
    , changed
    , getFetchedPixel
    , init
    , load
    , pixelCollected
    , sort
    , totalPrice
    , totalTax
    , totalUbi
    , updateByColor
    , updateByPixel
    , updateByPrice
    , updateByTransfer
    , updateByUbi
    )

import Array exposing (Array)
import BigInt
import Config exposing (getAssetsLimit, getAssetsPageLen, zeroPrice)
import Contract.Space
    exposing
        ( AssetsResultPage
        , ColorEvent
        , Pixel
        , PriceEvent
        , TransferEvent
        , UbiEvent
        )
import Data
    exposing
        ( BlockNumber
        , Index
        , Price
        , SortOrder(..)
        , WalletInfo(..)
        , dec
        , inc
        )
import Dict exposing (Dict)
import Eth.Types exposing (Address)
import Set exposing (Set)


type Assets
    = NotLoaded
    | Loading LoadingData
    | Loaded LoadedData


type alias LoadingData =
    { block : Maybe BlockNumber
    , loadedPages : LoadedPages
    , rank : Rank
    }


type alias LoadedPages =
    Maybe (Array LoadedPage)


type LoadedPage
    = Offset Int
    | Page AssetsResultPage


type alias LoadedData =
    { list : AssetsResult
    , rank : Rank
    , timeOrder : TimeOrder
    , total : Int
    , exceededLimit : Bool
    , changes : Changes
    , loadedIds : LoadedIds
    , fetchedPixels : FetchedPixels
    , collectedIds : CollectedIds
    }


type alias AssetsResult =
    List Pixel


type RankType
    = RankTime
    | RankPrice
    | RankTax
    | RankUbi


type alias Rank =
    ( RankType, SortOrder )


type alias TimeOrder =
    Dict Index Int


type alias Changes =
    Dict Index ChangeKind


type ChangeKind
    = Bought
    | Sold
    | Updated


type alias LoadedIds =
    Set Index


type alias FetchedPixels =
    Dict Index Pixel


type alias CollectedIds =
    Set Index



-- API


type alias LoadPageInfo =
    ( Address, Maybe BlockNumber, Int )


type Action
    = NoAction
    | LoadPage LoadPageInfo
    | Actions (List Action)


type alias Handler =
    WalletInfo -> Assets -> ( Assets, Action )


init : Assets
init =
    NotLoaded


load : Maybe BlockNumber -> Handler
load bk wallet assets =
    let
        load_ address =
            case address of
                Just addr ->
                    ( initLoadingData bk assets
                    , LoadPage ( addr, bk, 0 )
                    )

                _ ->
                    ( assets, NoAction )
    in
    case ( wallet, assets ) of
        ( Wallet { address }, NotLoaded ) ->
            load_ address

        ( Wallet { address }, Loaded _ ) ->
            load_ address

        _ ->
            ( assets, NoAction )


sort : Rank -> Assets -> Assets
sort rank assets =
    case assets of
        Loaded asts ->
            Loaded
                { asts
                    | list = asts.list |> sortAssetsResult rank asts.timeOrder
                    , rank = rank
                }

        _ ->
            assets


updateByPixel : Pixel -> Assets -> Assets
updateByPixel pixel assets =
    case assets of
        Loaded ({ changes, fetchedPixels, loadedIds, list, timeOrder } as asts) ->
            Loaded <|
                if Dict.get pixel.index changes == Just Bought then
                    let
                        newfpxls =
                            fetchedPixels |> Dict.insert pixel.index pixel

                        newTimeOrder =
                            let
                                topId =
                                    Dict.size timeOrder - 1 |> max 0
                            in
                            case timeOrder |> Dict.get topId of
                                Just topIdx ->
                                    if topIdx == pixel.index then
                                        timeOrder
                                            |> Dict.insert pixel.index topId

                                    else
                                        timeOrder
                                            |> Dict.insert
                                                pixel.index
                                                (inc topId)

                                Nothing ->
                                    timeOrder
                                        |> Dict.insert
                                            pixel.index
                                            (inc topId)

                        newAssets =
                            { asts
                                | fetchedPixels = newfpxls
                                , timeOrder = newTimeOrder
                            }
                    in
                    case
                        ( Dict.member pixel.index fetchedPixels
                        , Set.member pixel.index loadedIds
                        )
                    of
                        ( False, False ) ->
                            { newAssets
                                | list = pixel :: list
                                , loadedIds = Set.insert pixel.index loadedIds
                            }

                        _ ->
                            newAssets

                else
                    asts

        _ ->
            assets


updateByTransfer : WalletInfo -> TransferEvent -> Assets -> Assets
updateByTransfer wallet t assets =
    case ( wallet, assets ) of
        ( Wallet { address }, Loaded ({ changes, total } as asts) ) ->
            Loaded <|
                if address == Just t.from then
                    case changes |> Dict.get t.index of
                        Just Sold ->
                            asts

                        _ ->
                            { asts
                                | total = dec total |> max 0
                                , changes = Dict.insert t.index Sold changes
                            }

                else if address == Just t.to then
                    case changes |> Dict.get t.index of
                        Just Bought ->
                            asts

                        _ ->
                            { asts
                                | total = inc total
                                , changes = Dict.insert t.index Bought changes
                            }

                else
                    asts

        _ ->
            assets


updateByColor : ColorEvent -> Assets -> Assets
updateByColor { index } assets =
    case assets of
        Loaded ({ changes, loadedIds } as asts) ->
            Loaded <|
                if loadedBuNotChanged index changes loadedIds then
                    { asts | changes = changes |> Dict.insert index Updated }

                else
                    asts

        _ ->
            assets


updateByPrice : PriceEvent -> Assets -> Assets
updateByPrice { index } assets =
    case assets of
        Loaded ({ changes, loadedIds } as asts) ->
            Loaded <|
                if loadedBuNotChanged index changes loadedIds then
                    { asts | changes = changes |> Dict.insert index Updated }

                else
                    asts

        _ ->
            assets


updateByUbi : WalletInfo -> UbiEvent -> Assets -> Assets
updateByUbi wallet { index, collector } assets =
    case ( wallet, assets ) of
        ( Wallet { address }, Loaded ({ changes, collectedIds } as asts) ) ->
            if address == Just collector then
                Loaded <|
                    case changes |> Dict.get index of
                        Just Sold ->
                            asts

                        _ ->
                            { asts
                                | collectedIds =
                                    collectedIds |> Set.insert index
                            }

            else
                assets

        _ ->
            assets


addPage : AssetsResultPage -> Handler
addPage page wallet assets =
    case ( wallet, assets ) of
        ( Wallet { address }, Loading loadingInfo ) ->
            case address of
                Just addr ->
                    handleGetAssetsPage page addr loadingInfo

                _ ->
                    ( assets, NoAction )

        _ ->
            ( assets, NoAction )


totalPrice : LoadedData -> Price
totalPrice { list, fetchedPixels } =
    list
        |> List.foldr
            (\pixel total ->
                let
                    pxl =
                        case Dict.get pixel.index fetchedPixels of
                            Nothing ->
                                pixel

                            Just p ->
                                p
                in
                BigInt.add total pxl.price
            )
            zeroPrice


totalTax : LoadedData -> Price
totalTax assets =
    assets.list |> List.foldr (.tax >> BigInt.add) zeroPrice


totalUbi : LoadedData -> Price
totalUbi { list, collectedIds } =
    list
        |> List.filter (\pxl -> not <| Set.member pxl.index collectedIds)
        |> List.foldr (.ubi >> BigInt.add) zeroPrice


changed : LoadedData -> Bool
changed { changes, collectedIds } =
    not <| (Dict.isEmpty changes && Set.isEmpty collectedIds)


pixelCollected : Index -> CollectedIds -> Bool
pixelCollected idx collectedIds =
    collectedIds |> Set.member idx


getFetchedPixel : Index -> FetchedPixels -> Maybe Pixel
getFetchedPixel idx fetchedPixels =
    fetchedPixels |> Dict.get idx



-- Helpers


handleGetAssetsPage : AssetsResultPage -> Address -> LoadingData -> ( Assets, Action )
handleGetAssetsPage page addr loadingInfo =
    if justOnePage page then
        ( handleGetAssetsJustOnePage page loadingInfo, NoAction )

    else if firstPage page then
        let
            newLoadingInfo =
                if withinGetAssetsLimit page.total then
                    addOnePage page loadingInfo

                else
                    loadingInfo
        in
        handleGetOwnPixeRequestAllPages page addr newLoadingInfo

    else
        ( handleGetOwnPixeAddOnePage page loadingInfo, NoAction )


handleGetAssetsJustOnePage : AssetsResultPage -> LoadingData -> Assets
handleGetAssetsJustOnePage ({ pixels } as page) loadingInfo =
    assetsFinishLoading pixels page loadingInfo


handleGetOwnPixeRequestAllPages : AssetsResultPage -> Address -> LoadingData -> ( Assets, Action )
handleGetOwnPixeRequestAllPages page addr ({ block, loadedPages, rank } as loadingInfo) =
    let
        requestPage x =
            case x of
                Offset i ->
                    LoadPage ( addr, block, i )

                _ ->
                    NoAction
    in
    case loadedPages of
        Nothing ->
            let
                offsets =
                    calculateOffsets page.total
            in
            ( Loading
                { block = block
                , loadedPages = Just offsets
                , rank = rank
                }
            , Actions <| List.map requestPage <| Array.toList offsets
            )

        Just loaded_ ->
            ( Loading loadingInfo
            , Actions <| List.map requestPage <| Array.toList loaded_
            )


handleGetOwnPixeAddOnePage : AssetsResultPage -> LoadingData -> Assets
handleGetOwnPixeAddOnePage page loadingInfo =
    let
        newLoadingInfo =
            addOnePage page loadingInfo

        newAssets =
            Loading newLoadingInfo
    in
    if allPageLoaded newLoadingInfo.loadedPages then
        case newLoadingInfo.loadedPages of
            Nothing ->
                newAssets

            Just loadedPages ->
                let
                    aux a b =
                        case a of
                            Offset _ ->
                                b

                            Page p ->
                                b ++ p.pixels

                    pixels =
                        Array.foldl aux [] loadedPages
                in
                assetsFinishLoading pixels page newLoadingInfo

    else
        newAssets


initLoadingData : Maybe BlockNumber -> Assets -> Assets
initLoadingData bk assets =
    Loading { block = bk, loadedPages = Nothing, rank = getRank assets }


assetsFinishLoading : AssetsResult -> AssetsResultPage -> LoadingData -> Assets
assetsFinishLoading pixels page loadingInfo =
    let
        rank =
            loadingInfo.rank

        timeOrder =
            pixelsToTimeOrder pixels
    in
    Loaded
        { list = pixels |> sortAssetsResult rank timeOrder
        , rank = rank
        , timeOrder = timeOrder
        , total = page.total
        , exceededLimit = not <| withinGetAssetsLimit page.total
        , changes = Dict.empty
        , loadedIds = pixels |> List.map .index |> Set.fromList
        , fetchedPixels = Dict.empty
        , collectedIds = Set.empty
        }


loadedBuNotChanged : Index -> Changes -> LoadedIds -> Bool
loadedBuNotChanged idx cs ids =
    Set.member idx ids && (not <| Dict.member idx cs)


withinGetAssetsLimit : Int -> Bool
withinGetAssetsLimit i =
    i <= getAssetsLimit


defaultAssestsSort : Rank
defaultAssestsSort =
    ( RankTime, Descend )


getRank : Assets -> Rank
getRank assets =
    case assets of
        NotLoaded ->
            defaultAssestsSort

        Loading { rank } ->
            rank

        Loaded { rank } ->
            rank


pixelsToTimeOrder : List Pixel -> TimeOrder
pixelsToTimeOrder pxls =
    pxls |> List.indexedMap (\i pxl -> ( pxl.index, i )) |> Dict.fromList


sortAssetsResult : Rank -> TimeOrder -> AssetsResult -> AssetsResult
sortAssetsResult rank timeOrder =
    let
        sortWithTime order =
            case order of
                Ascend ->
                    List.sortWith compareTime

                Descend ->
                    List.sortWith flippedCompareTime

        compareTime a b =
            case ( Dict.get a.index timeOrder, Dict.get b.index timeOrder ) of
                ( Nothing, Nothing ) ->
                    EQ

                ( Just _, Nothing ) ->
                    LT

                ( Nothing, Just _ ) ->
                    GT

                ( Just x, Just y ) ->
                    compare x y

        flippedCompareTime a b =
            compareTime b a

        sortWithBigIntAttribute attr order =
            let
                cmp x y =
                    case order of
                        Ascend ->
                            case BigInt.compare (attr x) (attr y) of
                                EQ ->
                                    compare (.index x) (.index y)

                                r ->
                                    r

                        Descend ->
                            case BigInt.compare (attr y) (attr x) of
                                EQ ->
                                    compare (.index x) (.index y)

                                r ->
                                    r
            in
            List.sortWith <| \x y -> cmp x y
    in
    case rank of
        ( RankTime, order ) ->
            sortWithTime order

        ( RankPrice, order ) ->
            sortWithBigIntAttribute .price order

        ( RankTax, order ) ->
            sortWithBigIntAttribute .tax order

        ( RankUbi, order ) ->
            sortWithBigIntAttribute .ubi order


allPageLoaded : LoadedPages -> Bool
allPageLoaded loadedPages_ =
    let
        isOffset x =
            case x of
                Offset _ ->
                    True

                _ ->
                    False
    in
    case loadedPages_ of
        Nothing ->
            False

        Just loadedPages ->
            loadedPages |> Array.filter isOffset |> Array.isEmpty


addOnePage : AssetsResultPage -> LoadingData -> LoadingData
addOnePage page loadingInfo =
    let
        pageN =
            whichPage page

        page_ =
            Page <| { page | pixels = page.pixels }

        offsets =
            case loadingInfo.loadedPages of
                Nothing ->
                    calculateOffsets page.total

                Just loadedPages ->
                    loadedPages
    in
    { loadingInfo | loadedPages = Just <| Array.set pageN page_ offsets }


justOnePage : AssetsResultPage -> Bool
justOnePage page =
    page.total <= page.limit


firstPage : AssetsResultPage -> Bool
firstPage page =
    page.offset == 0


whichPage : AssetsResultPage -> Int
whichPage page =
    let
        realOffset =
            if withinGetAssetsLimit page.total then
                page.offset

            else
                page.offset - page.total + getAssetsLimit
    in
    realOffset // getAssetsPageLen


calculateOffsets : Int -> Array.Array LoadedPage
calculateOffsets total =
    if total > getAssetsLimit then
        let
            pageNum =
                calculatePageNum getAssetsLimit getAssetsPageLen

            start =
                total - getAssetsLimit
        in
        Array.initialize pageNum (\i -> Offset <| i * getAssetsPageLen + start)

    else
        let
            pageNum =
                calculatePageNum total getAssetsPageLen
        in
        Array.initialize pageNum (\i -> Offset <| i * getAssetsPageLen)


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
