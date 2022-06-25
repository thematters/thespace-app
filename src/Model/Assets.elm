module Model.Assets exposing
    ( AssetChangeKind(..)
    , Assets(..)
    , AssetsChanges
    , AssetsCollectedIds
    , AssetsFetched
    , AssetsResult
    , AssetsSort
    , AssetsSortType(..)
    , LoadedAssetsData
    , LoadedAssetsPage(..)
    , LoadedAssetsPages
    , LoadingAssets
    , assetsFinishLoading
    , getFetchedPixel
    , initLoadingAssets
    , loadedAssetsChanged
    , loadedAssetsTotalPrice
    , loadedAssetsTotalTax
    , loadedAssetsTotalUbi
    , pixelCollected
    , sortAssetsResult
    , updateAssetsByColor
    , updateAssetsByPixel
    , updateAssetsByPrice
    , updateAssetsByTransfer
    , updateAssetsByUbi
    , withinGetOwnPixelLimit
    )

import Array exposing (Array)
import BigInt
import Config exposing (getOwnPixelLimit, zeroPrice)
import Contract.Space
    exposing
        ( ColorEvent
        , PriceEvent
        , TransferEvent
        , UbiEvent
        )
import Data
    exposing
        ( BlockNumber
        , Index
        , OwnPixelsResultPage
        , Pixel
        , Price
        , SortOrder(..)
        , dec
        , inc
        )
import Dict exposing (Dict)
import Eth.Types exposing (Address)
import Set exposing (Set)


type AssetChangeKind
    = Bought
    | Sold
    | Updated


type Assets
    = AssetsNotLoaded
    | AssetsLoading LoadingAssets
    | AssetsLoaded LoadedAssets


type alias LoadingAssets =
    { block : Maybe BlockNumber
    , loaded : LoadedAssetsPages
    , sort : AssetsSort
    }


type alias LoadedAssetsPages =
    Maybe (Array LoadedAssetsPage)


type LoadedAssetsPage
    = Offset Int
    | Page OwnPixelsResultPage


type alias LoadedAssets =
    LoadedAssetsData


type alias LoadedAssetsData =
    { list : AssetsResult
    , sort : AssetsSort
    , timeOrder : AssetsTiemOrder
    , total : Int
    , exceededLimit : Bool
    , changes : AssetsChanges
    , loadedIds : AssetsLoadedIds
    , fetchedPixels : AssetsFetched
    , collectedIds : AssetsCollectedIds
    }


type alias AssetsResult =
    List Pixel


type AssetsSortType
    = AssetsSortTime
    | AssetsSortPrice
    | AssetsSortTax
    | AssetsSortUbi


type alias AssetsSort =
    ( AssetsSortType, SortOrder )


type alias AssetsTiemOrder =
    Dict Index Int


type alias AssetsChanges =
    Dict Index AssetChangeKind


type alias AssetsLoadedIds =
    Set Index


type alias AssetsFetched =
    Dict Index Pixel


type alias AssetsCollectedIds =
    Set Index


initLoadingAssets : Maybe BlockNumber -> Assets -> Assets
initLoadingAssets bk assets =
    AssetsLoading { block = bk, loaded = Nothing, sort = getAssetsSort assets }


assetsFinishLoading : AssetsResult -> OwnPixelsResultPage -> Assets -> Assets
assetsFinishLoading pixels page assets =
    let
        sort =
            getAssetsSort assets

        timeOrder =
            pixelsToTimeOrder pixels
    in
    AssetsLoaded
        { list = pixels |> sortAssetsResult sort timeOrder
        , sort = sort
        , timeOrder = timeOrder
        , total = page.total
        , exceededLimit = not <| withinGetOwnPixelLimit page.total
        , changes = Dict.empty
        , loadedIds = pixels |> List.map .index |> Set.fromList
        , fetchedPixels = Dict.empty
        , collectedIds = Set.empty
        }


loadedAssetsTotalPrice : LoadedAssetsData -> Price
loadedAssetsTotalPrice { list, fetchedPixels } =
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


loadedAssetsTotalTax : LoadedAssetsData -> Price
loadedAssetsTotalTax assets =
    assets.list |> List.foldr (.tax >> BigInt.add) zeroPrice


loadedAssetsTotalUbi : LoadedAssetsData -> Price
loadedAssetsTotalUbi { list, collectedIds } =
    list
        |> List.filter (\pxl -> not <| Set.member pxl.index collectedIds)
        |> List.foldr (.ubi >> BigInt.add) zeroPrice


loadedAssetsChanged : LoadedAssetsData -> Bool
loadedAssetsChanged { changes, collectedIds } =
    not <| (Dict.isEmpty changes && Set.isEmpty collectedIds)


updateAssetsByPixel : Pixel -> LoadedAssetsData -> LoadedAssetsData
updateAssetsByPixel pixel ({ changes, fetchedPixels, loadedIds, list, timeOrder } as assets) =
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
                            timeOrder |> Dict.insert pixel.index topId

                        else
                            timeOrder |> Dict.insert pixel.index (inc topId)

                    Nothing ->
                        timeOrder |> Dict.insert pixel.index (inc topId)

            newAssets =
                { assets | fetchedPixels = newfpxls, timeOrder = newTimeOrder }
        in
        case
            ( Dict.member pixel.index fetchedPixels
            , Set.member pixel.index loadedIds
            )
        of
            ( False, False ) ->
                { newAssets
                    | list = pixel :: list
                    , loadedIds = loadedIds |> Set.insert pixel.index
                }

            _ ->
                newAssets

    else
        assets


updateAssetsByTransfer : Maybe Address -> TransferEvent -> LoadedAssetsData -> LoadedAssetsData
updateAssetsByTransfer address transfer ({ changes, total } as assets) =
    if address == Just transfer.from then
        case changes |> Dict.get transfer.index of
            Just Sold ->
                assets

            _ ->
                { assets
                    | total = dec total |> max 0
                    , changes = changes |> Dict.insert transfer.index Sold
                }

    else if address == Just transfer.to then
        case changes |> Dict.get transfer.index of
            Just Bought ->
                assets

            _ ->
                { assets
                    | total = inc total
                    , changes = changes |> Dict.insert transfer.index Bought
                }

    else
        assets


loadedBuNotChanged : Index -> AssetsChanges -> AssetsLoadedIds -> Bool
loadedBuNotChanged idx cs ids =
    Set.member idx ids && (not <| Dict.member idx cs)


updateAssetsByColor : ColorEvent -> LoadedAssetsData -> LoadedAssetsData
updateAssetsByColor { index } ({ changes, loadedIds } as assets) =
    if loadedBuNotChanged index changes loadedIds then
        { assets | changes = changes |> Dict.insert index Updated }

    else
        assets


updateAssetsByPrice : PriceEvent -> LoadedAssetsData -> LoadedAssetsData
updateAssetsByPrice { index } ({ changes, loadedIds } as assets) =
    if loadedBuNotChanged index changes loadedIds then
        { assets | changes = changes |> Dict.insert index Updated }

    else
        assets


updateAssetsByUbi : UbiEvent -> LoadedAssetsData -> LoadedAssetsData
updateAssetsByUbi { index } ({ changes, collectedIds } as assets) =
    case changes |> Dict.get index of
        Just Sold ->
            assets

        _ ->
            { assets | collectedIds = collectedIds |> Set.insert index }


getFetchedPixel : Index -> AssetsFetched -> Maybe Pixel
getFetchedPixel idx fetchedPixels =
    fetchedPixels |> Dict.get idx


pixelCollected : Index -> AssetsCollectedIds -> Bool
pixelCollected idx collectedIds =
    collectedIds |> Set.member idx


withinGetOwnPixelLimit : Int -> Bool
withinGetOwnPixelLimit i =
    i <= getOwnPixelLimit


defaultAssestsSort : AssetsSort
defaultAssestsSort =
    ( AssetsSortTime, Descend )


getAssetsSort : Assets -> AssetsSort
getAssetsSort assets =
    case assets of
        AssetsNotLoaded ->
            defaultAssestsSort

        AssetsLoading { sort } ->
            sort

        AssetsLoaded { sort } ->
            sort


pixelsToTimeOrder : List Pixel -> AssetsTiemOrder
pixelsToTimeOrder pxls =
    pxls |> List.indexedMap (\i pxl -> ( pxl.index, i )) |> Dict.fromList


sortAssetsResult : AssetsSort -> AssetsTiemOrder -> AssetsResult -> AssetsResult
sortAssetsResult sort timeOrder =
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
                cmp =
                    case order of
                        Ascend ->
                            BigInt.compare

                        Descend ->
                            flippedBigIntCompare
            in
            List.sortWith <| \x y -> cmp (attr x) (attr y)

        flippedBigIntCompare a b =
            BigInt.compare b a
    in
    case sort of
        ( AssetsSortTime, order ) ->
            sortWithTime order

        ( AssetsSortPrice, order ) ->
            sortWithBigIntAttribute .price order

        ( AssetsSortTax, order ) ->
            sortWithBigIntAttribute .tax order

        ( AssetsSortUbi, order ) ->
            sortWithBigIntAttribute .ubi order
