module View.Sidebar exposing (viewSidebar)

import Array
import Config
    exposing
        ( getOwnPixelLimit
        , lightColor
        , price
        , sidebarWidth
        , sidebarzIndex
        , tokenSign
        , zeroPrice
        )
import Contract.ERC20 exposing (allowance)
import Css exposing (..)
import Data
    exposing
        ( Activity(..)
        , ColorId
        , Index
        , Pixel
        , Price
        , RpcErrorKind(..)
        , Size
        , SortOrder(..)
        , WalletInfo(..)
        , fromWei
        , notOnChain
        , tokenDeposited
        , unsafeColorIdToHexColor
        , walletConnected
        , walletIsAddress
        )
import Data.Icon as Icon
import Data.Image exposing (logoDataUri)
import Dict
import Env exposing (env)
import Eth.Defaults exposing (zeroAddress)
import Eth.Units exposing (EthUnit(..))
import Html
import Html.Styled exposing (Html, button, div, img, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href, src, title)
import Html.Styled.Events exposing (onClick)
import InfiniteList as Inf
import Model
    exposing
        ( AppMode(..)
        , SidebarInfLists
        , SidebarInfoType(..)
        , SidebarMode
        , SidebarUIMode(..)
        , TaxInfo
        )
import Model.Assets
    exposing
        ( AssetChangeKind(..)
        , Assets(..)
        , AssetsChanges
        , AssetsCollectedIds
        , AssetsFetched
        , AssetsSort
        , AssetsSortType(..)
        , LoadedAssetsData
        , LoadedAssetsPage(..)
        , getFetchedPixel
        , loadedAssetsChanged
        , loadedAssetsTotalPrice
        , loadedAssetsTotalTax
        , loadedAssetsTotalUbi
        , pixelCollected
        )
import Msg exposing (Msg(..))
import View.Common exposing (..)



-- Sizes


edge : Float
edge =
    24


navHeight : Float
navHeight =
    40


acctInfoHeight : Float
acctInfoHeight =
    94


switchHeight : Float
switchHeight =
    20


heightWithoutLogs : Float
heightWithoutLogs =
    navHeight + acctInfoHeight + switchHeight + 20


entryHeight : Float
entryHeight =
    64


assetOpHeight : Float
assetOpHeight =
    60



-- UI Helpers


coordinates : Bool -> Index -> Html Msg
coordinates prefix idx =
    div
        [ css [ cursor pointer, color secondary ]
        , onClick <| SelectCell ( idx, True )
        ]
        [ View.Common.coords secDiv prefix idx ]


box : ColorId -> Html msg
box colorId =
    let
        boxSize =
            25

        hexc =
            unsafeColorIdToHexColor colorId

        style_ =
            if lightColor hexc then
                [ displayFlex
                , backgroundColor (hex hexc)
                , width (px <| boxSize - 2)
                , height (px <| boxSize - 2)
                , border3 (px 1) solid bggray
                ]

            else
                [ displayFlex
                , backgroundColor (hex hexc)
                , width (px boxSize)
                , height (px boxSize)
                ]
    in
    div [ css style_ ] []


placeholder : List (Html msg) -> Html msg
placeholder e =
    let
        style_ =
            [ displayFlex
            , justifyContent center
            , marginTop (px 40)
            , width (pct 100)
            , color gray
            ]
    in
    div [ css style_ ] e



-- Infinite List


scrollAreaStyle : Float -> Int -> List Style
scrollAreaStyle areaH itemLength =
    let
        showScrollBar =
            (itemLength |> toFloat) * entryHeight > areaH

        leftMargin =
            if showScrollBar then
                edge - scrollBarWidth

            else
                edge
    in
    [ height (px areaH)
    , overflowX hidden
    , overflowY auto
    , property "-webkit-overflow-scrolling" "touch"
    , paddingLeft (px edge)
    , paddingRight (px leftMargin)
    ]


scrollConfig : Float -> (Int -> Int -> item -> Html.Html msg) -> Inf.Config item msg
scrollConfig areaH itemView =
    Inf.config
        { itemView = itemView
        , itemHeight = entryHeight |> floor |> Inf.withConstantHeight
        , containerHeight = floor areaH
        }


scrollArea : Float -> List item -> Inf.Model -> (Int -> Int -> item -> Html.Html msg) -> (Inf.Model -> msg) -> Html msg
scrollArea scrollH items infList itemView msg =
    div
        [ css <| scrollAreaStyle scrollH <| List.length items
        , Inf.onScroll msg |> Html.Styled.Attributes.fromUnstyled
        ]
        [ Inf.view (scrollConfig scrollH itemView) infList items
            |> Html.Styled.fromUnstyled
        ]



-- Views


viewSidebar : SidebarMode -> Size -> WalletInfo -> List Activity -> Assets -> SidebarInfLists -> TaxInfo -> Html Msg
viewSidebar ( uiMode, infoType ) winSize wallet acts assets { actsInfList, assetsInfList } { mintTax } =
    let
        modalW =
            sidebarWidth

        modalH =
            let
                ( _, winH ) =
                    winSize
            in
            toFloat winH - 2 * modalEdge

        baseStyle =
            [ position absolute
            , zIndex (int sidebarzIndex)
            , top (px modalEdge)
            , left (px modalEdge)
            , width (px modalW)
            , modalBoxShadow
            , modalRadius
            , backgroundColor modalBackgroundColor
            ]

        collapsedStyle =
            height (px <| edge * 2 + navHeight)
                :: baseStyle

        expandedStyle =
            height (px modalH) :: baseStyle
    in
    case uiMode of
        CollapsedSidebar ->
            div
                [ css collapsedStyle ]
                [ div
                    [ css [ margin (px edge), marginBottom (px 0) ] ]
                    [ nav uiMode ]
                ]

        ExpandedSidebar ->
            div
                [ css expandedStyle ]
                [ div
                    [ css
                        [ margin (px edge)
                        , marginBottom (px 0)
                        , height (px heightWithoutLogs)
                        ]
                    ]
                    [ nav uiMode
                    , acctInfo wallet
                    , switch wallet infoType assets
                    ]
                , let
                    acts_ =
                        viewActs modalH wallet mintTax actsInfList acts

                    assets_ =
                        viewAssets modalH wallet assetsInfList assets
                  in
                  case infoType of
                    -- Some trick as canvas, using fixed layout
                    -- to avoid scroll reset problem
                    ActLogs ->
                        div []
                            [ div [] [ acts_ ]
                            , div [ css [ display none ] ] [ assets_ ]
                            ]

                    AssetsManager ->
                        div []
                            [ div [ css [ display none ] ] [ acts_ ]
                            , div [] [ assets_ ]
                            ]
                ]


nav : SidebarUIMode -> Html Msg
nav uiMode =
    let
        style_ =
            [ displayFlex
            , justifyContent spaceBetween
            , alignItems start
            , height (px navHeight)
            ]

        iconsStyle =
            [ displayFlex, property "gap" "10px", marginTop (px -4) ]

        logo =
            img [ src logoDataUri, css [ height (px navHeight) ] ] []

        linkIcon url title_ icon =
            div [ css [ cursor pointer ], title title_ ]
                [ Html.Styled.a
                    [ css [ outline none ]
                    , Html.Styled.Attributes.target "_blank"
                    , href url
                    ]
                    [ iconNormal icon ]
                ]

        info_ =
            linkIcon Config.aboutLink "About" Icon.info

        discord =
            linkIcon Config.discordLink "Discord" Icon.discord

        --playback =
        --    div
        --        [ css [ cursor pointer ]
        --        , title "Playback Recent Changes"
        --        , onClick <| AppModeChange PlaybackLoading
        --        ]
        --        [ iconNormal Icon.clock ]
        modeSwitch =
            div
                [ css [ cursor pointer ]
                , onClick <|
                    SidebarModeSwitch <|
                        case uiMode of
                            CollapsedSidebar ->
                                ExpandedSidebar

                            ExpandedSidebar ->
                                CollapsedSidebar
                ]
                [ iconNormal <|
                    case uiMode of
                        CollapsedSidebar ->
                            Icon.activity

                        ExpandedSidebar ->
                            Icon.chevronUp
                ]

        icons =
            div [ css iconsStyle ]
                [ info_
                , discord
                , modeSwitch

                --, playback
                ]
    in
    div
        [ css style_ ]
        [ logo
        , div [ css [ color secondary ] ] [ icons ]
        ]


acctInfo : WalletInfo -> Html Msg
acctInfo wallet =
    let
        style_ =
            [ displayFlex
            , flexDirection column
            , alignItems center
            , justifyContent spaceAround
            , height (px <| acctInfoHeight)
            ]

        info addr blc =
            let
                containerStyle =
                    [ displayFlex
                    , flexDirection column
                    , justifyContent spaceBetween
                    , height (pct 48)
                    , width (pct 100)
                    ]

                ln label value =
                    div [ css [ displayFlex, justifyContent spaceBetween ] ]
                        [ label, value ]
            in
            div
                [ css containerStyle ]
                [ ln (secDiv "My Wallet") (greenAddressTag addr)
                , ln (secDiv "Balance") (priceTagNormal blc)
                ]
    in
    div
        [ css style_ ]
        [ case wallet of
            DetectingWallet ->
                loadingMetaMaskBtn

            NoWallet ->
                installMetaMaskBtn

            LockedWallet ->
                lockedMataMaskBtn

            Wallet { chainId, address, balance, allowance } ->
                if notOnChain chainId then
                    switchNetworkBtn

                else
                    case ( address, balance, allowance ) of
                        ( Nothing, _, _ ) ->
                            connectMetaMaskBtn

                        ( _, Nothing, _ ) ->
                            loadingMetaMaskBtn

                        ( _, _, Nothing ) ->
                            loadingMetaMaskBtn

                        ( Just addr, Just blc, Just alw ) ->
                            if blc == zeroPrice then
                                getTokenBtn

                            else if tokenDeposited alw then
                                info addr blc

                            else
                                depositBtn
        ]


switch : WalletInfo -> SidebarInfoType -> Assets -> Html Msg
switch wallet activeInfoType assets =
    let
        style_ =
            [ displayFlex
            , justifyContent spaceBetween
            , height (px switchHeight)
            , marginBottom (px 20)
            ]

        toggleStyle disabled changed =
            [ displayFlex
            , justifyContent center
            , width (pct 50)
            , pseudoClass "first-child" [ borderRight3 (px 2) solid secondary ]
            ]
                ++ (if disabled then
                        [ color lightgray ]

                    else if changed then
                        [ color green, cursor pointer ]

                    else
                        [ cursor pointer ]
                   )

        toggle t disabled changed name =
            let
                tglBaseStyle =
                    toggleStyle disabled changed

                tglStyle =
                    if t == activeInfoType then
                        fontWeight bold :: tglBaseStyle

                    else
                        tglBaseStyle

                attrs =
                    if disabled then
                        [ css tglStyle ]

                    else
                        [ css tglStyle, onClick <| SidebarInfoSwitch t ]
            in
            div attrs [ text name ]

        yourPixelsNotAvailable =
            not <| walletConnected wallet

        yourPixelsChanged =
            case assets of
                AssetsLoaded ast ->
                    loadedAssetsChanged ast

                _ ->
                    False
    in
    div
        [ css style_ ]
        [ toggle ActLogs False False "Activity"
        , toggle AssetsManager yourPixelsNotAvailable yourPixelsChanged "My Pixels"
        ]


viewActs : Float -> WalletInfo -> Maybe Price -> Inf.Model -> List Activity -> Html Msg
viewActs modalH wallet mintTax actsInfList acts =
    if List.length acts == 0 then
        placeholder <| [ textDiv "No new activity recieved yet." ]

    else
        let
            scrollH =
                modalH - heightWithoutLogs - edge

            itemView =
                \_ _ act -> viewActEntry wallet mintTax act |> toUnstyled
        in
        scrollArea scrollH acts actsInfList itemView ScrollActivity


viewActEntry : WalletInfo -> Maybe Price -> Activity -> Html Msg
viewActEntry wallet mintTax log =
    let
        style_ =
            [ displayFlex
            , flexDirection column
            , justifyContent center
            , height (px entryHeight)
            , borderBottom3 (px 1) solid lightbggray
            ]

        cssLog removed =
            let
                scratched =
                    if removed then
                        [ textDecoration lineThrough ]

                    else
                        []
            in
            css <| style_ ++ scratched

        lineMarginTop =
            4

        lineStyle =
            [ displayFlex
            , alignItems center
            , property "gap" "6px"
            , marginTop (px lineMarginTop)
            ]

        addr a =
            if walletIsAddress wallet a then
                greenAddressTag a

            else
                addressTag a

        coords =
            coordinates False

        ln es =
            div [ css lineStyle ] es

        transferLog idx from_ to_ p =
            [ if walletIsAddress wallet from_ then
                ln [ coords idx, grayDiv "bought from", addr from_ ]

              else
                ln [ addr to_, grayDiv "bought", coords idx ]
            , ln [ priceTag p ]
            ]

        colorLog idx owr c =
            [ div [ css [ displayFlex, justifyContent spaceBetween ] ]
                [ div [] [ ln [ addr owr, grayDiv "colored", coords idx ] ]
                , div [ css [ displayFlex, alignItems center ] ] [ box c ]
                ]
            ]

        priceResetLog idx p =
            [ ln [ grayDiv "TheSpace priced", coords idx ], ln [ priceTag p ] ]

        priceLog idx owr p =
            [ ln [ addr owr, grayDiv "priced", coords idx ]
            , ln [ priceTag p ]
            ]

        taxLog idx pyer amt =
            [ ln [ coords idx, grayDiv "taxed", addr pyer ]
            , ln [ priceTag amt ]
            ]

        incomeLog idx coller amt =
            [ ln [ addr coller, grayDiv "collected", coords idx ]
            , ln [ priceTag amt ]
            ]

        defaultLog idx from_ =
            let
                defaultLn =
                    ln [ coords idx, grayDiv "defaulted from", addr from_ ]
            in
            case mintTax of
                Just p ->
                    [ defaultLn, ln [ grayDiv "to", priceTag p ] ]

                Nothing ->
                    [ defaultLn ]

        requestLog idx =
            [ ln [ grayDiv "Request sent for", coords idx ] ]

        errorLog { code, message } =
            if env.debug then
                let
                    msg =
                        message |> String.trim |> String.left 40
                in
                [ ln
                    [ boldTextDiv <| redDiv <| "Error "
                    , secDiv <| "code " ++ String.fromInt code
                    ]
                , div
                    [ css <| lineStyle, title message ]
                    [ secDiv <| msg ++ "…" ]
                ]

            else
                --case kind of
                --    RpcUnderPricedError idx ->
                --        [ ln
                --            [ redDiv "Under priced"
                --            , secDiv "when buying"
                --            , coords idx
                --            ]
                --        , ln
                --            [ Html.Styled.a
                --                [ css
                --                    [ color gray
                --                    , fontSize smallText
                --                    , textDecoration none
                --                    ]
                --                , Html.Styled.Attributes.target "_blank"
                --                , href underPricedHelpLink
                --                ]
                --                [ text "What's this and what can I do?" ]
                --            ]
                --        ]
                --    RpcUnknownError ->
                [ ln [ redDiv "Huh...something seems wrong here." ] ]
    in
    case log of
        TransferAct { index, from, to, amount, removed } ->
            div
                [ cssLog removed ]
            <|
                transferLog index from to amount

        ColorAct { index, color, owner, removed } ->
            div
                [ cssLog removed ]
            <|
                colorLog index owner color

        PriceAct { index, price, owner, removed } ->
            div
                [ cssLog removed ]
            <|
                if owner == zeroAddress then
                    priceResetLog index price

                else
                    priceLog index owner price

        TaxAct { index, payer, amount, removed } ->
            div [ cssLog removed ] <| taxLog index payer amount

        UbiAct { index, collector, amount, removed } ->
            div [ cssLog removed ] <| incomeLog index collector amount

        DefaultAct { from, index, removed } ->
            div [ cssLog removed ] <| defaultLog index from

        TxSendAct index ->
            div [ cssLog False ] <| requestLog index

        ActError err ->
            div [ cssLog False ] <| errorLog err


viewAssets : Float -> WalletInfo -> Inf.Model -> Assets -> Html Msg
viewAssets modalH wallet actInfList assets =
    if walletConnected wallet then
        case assets of
            AssetsNotLoaded ->
                placeholder <|
                    [ div [ css [ displayFlex, flexDirection column ] ]
                        [ div
                            [ css [ marginTop (px 10), marginBottom (px 10) ] ]
                            [ text "Your pixels are not loaded." ]
                        , button
                            [ css buttonStyle, onClick LoadAssets ]
                            [ text "Load Now" ]
                        ]
                    ]

            AssetsLoading { loaded } ->
                let
                    per =
                        case loaded of
                            Nothing ->
                                ""

                            Just loaded_ ->
                                let
                                    hasLoaded x =
                                        case x of
                                            Page _ ->
                                                True

                                            _ ->
                                                False

                                    needLoadPages =
                                        loaded_ |> Array.length

                                    loadedPages =
                                        loaded_
                                            |> Array.filter hasLoaded
                                            |> Array.length

                                    percent =
                                        loadedPages * 100 // needLoadPages
                                in
                                String.fromInt percent ++ "%"
                in
                placeholder <|
                    [ div
                        [ css
                            [ displayFlex
                            , flexDirection column
                            , textAlign center
                            ]
                        ]
                        [ spinner bigTextSize grayStr
                        , div
                            [ css [ marginTop (px 10) ] ]
                            [ textDiv "Loading your pixels, hang on..."
                            , textDiv per
                            ]
                        ]
                    ]

            AssetsLoaded ast ->
                if List.length ast.list == 0 then
                    placeholder <| [ textDiv "You don't have any pixels yet." ]

                else
                    div []
                        [ viewAssetOps ast
                        , viewAssetsResult modalH ast actInfList
                        ]

    else
        placeholder [ textDiv "Please connect wallet first." ]


viewAssetOps : LoadedAssetsData -> Html Msg
viewAssetOps ({ total, exceededLimit, sort } as assets) =
    let
        bottomMargin =
            10

        refresh_ icon =
            div
                [ css [ displayFlex, cursor pointer, property "gap" "4px" ]
                , onClick RefreshAssets
                ]
                [ icon Icon.refreshSmall ]

        nomalRefresh =
            refresh_ iconLight

        changedRefresh =
            refresh_ iconGreen

        refresh asts =
            if loadedAssetsChanged asts then
                changedRefresh

            else
                nomalRefresh

        totalCount ttl =
            let
                totalNum =
                    if ttl == 0 || ttl == 1 then
                        secDiv <| "Total " ++ String.fromInt ttl ++ " Pixel"

                    else
                        secDiv <| "Total " ++ String.fromInt ttl ++ " Pixels"
            in
            if exceededLimit then
                let
                    showingStr =
                        "(showing " ++ String.fromInt getOwnPixelLimit ++ ")"

                    showing =
                        smallTextDiv <| grayDiv <| showingStr
                in
                div
                    [ css [ displayFlex, alignItems end, property "gap" "4px" ] ]
                    [ totalNum, showing ]

            else
                totalNum

        totalMonetaryInfo lbl ttl =
            div
                [ css [ displayFlex, alignItems end, property "gap" "4px" ] ]
                [ secDiv lbl, priceTagNormal ttl ]

        totalInfo =
            case sort of
                ( AssetsSortTime, _ ) ->
                    totalCount total

                ( AssetsSortPrice, _ ) ->
                    totalMonetaryInfo "Total" <| loadedAssetsTotalPrice assets

                ( AssetsSortTax, _ ) ->
                    totalMonetaryInfo "Tax ≈" <| loadedAssetsTotalTax assets

                ( AssetsSortUbi, _ ) ->
                    totalMonetaryInfo "Inc. ≈" <| loadedAssetsTotalUbi assets

        info =
            div
                [ css
                    [ displayFlex
                    , alignItems center
                    , justifyContent spaceBetween
                    ]
                ]
                [ totalInfo, refresh assets ]
    in
    div
        [ css
            [ displayFlex
            , flexDirection column
            , justifyContent spaceBetween
            , height (px <| assetOpHeight - bottomMargin)
            , margin4 (px 0) (px edge) (px bottomMargin) (px edge)
            ]
        ]
        [ viewAssetsSorts sort, info ]


viewAssetsSorts : AssetsSort -> Html Msg
viewAssetsSorts ( sType, sOrder ) =
    let
        item sortType label =
            div
                [ css [ displayFlex, alignItems end, cursor pointer ]
                , onClick <|
                    SortAssets
                        ( sortType
                        , if sortType == sType && sOrder == Descend then
                            Ascend

                          else
                            Descend
                        )
                ]
                [ div
                    [ css [ marginRight (px 2) ] ]
                    [ if sortType == sType then
                        secDiv label

                      else
                        grayDiv label
                    ]
                , case sOrder of
                    Descend ->
                        indicator ( sortType, Descend ) "▼"

                    Ascend ->
                        indicator ( sortType, Ascend ) "▲"
                ]

        indicator indicatorSortInfo label =
            div
                [ css [ fontSize smallText ] ]
                [ if indicatorSortInfo == ( sType, sOrder ) then
                    secDiv label

                  else
                    phantomDiv
                ]
    in
    div
        [ css
            [ displayFlex
            , alignItems center
            , justifyContent spaceBetween
            , marginBottom (px 10)
            ]
        ]
        [ item AssetsSortTime "Recent"
        , item AssetsSortPrice "Price"
        , item AssetsSortTax "Tax"
        , item AssetsSortUbi "Inc."
        ]


viewAssetsResult : Float -> LoadedAssetsData -> Inf.Model -> Html Msg
viewAssetsResult modalH { sort, changes, fetchedPixels, collectedIds, list } assetsInfList =
    let
        scrollH =
            modalH - (heightWithoutLogs + edge + assetOpHeight)

        itemView =
            \_ _ p ->
                viewAssetResultEntry sort changes fetchedPixels collectedIds p
                    |> toUnstyled
    in
    scrollArea scrollH list assetsInfList itemView ScrollAssets


viewAssetResultEntry : AssetsSort -> AssetsChanges -> AssetsFetched -> AssetsCollectedIds -> Pixel -> Html Msg
viewAssetResultEntry ( sortType, _ ) changes fetchedPixels collectedIds pixel =
    let
        ( pxl, tradeFlag ) =
            case Dict.get pixel.index changes of
                Just Bought ->
                    let
                        fetchPxl =
                            case getFetchedPixel pixel.index fetchedPixels of
                                Just p ->
                                    p

                                Nothing ->
                                    pixel
                    in
                    ( fetchPxl, boldTextDiv <| smallTextDiv <| greenDiv "New" )

                Just Sold ->
                    ( pixel, boldTextDiv <| smallTextDiv <| orangeDiv "Sold" )

                Just Updated ->
                    ( pixel, smallTextDiv <| greenDiv "Updated" )

                Nothing ->
                    ( pixel, phantomDiv )

        ( extraTag, value ) =
            case sortType of
                AssetsSortTax ->
                    ( secDiv "Tax ≈", priceTag pxl.tax )

                AssetsSortUbi ->
                    ( if pixelCollected pxl.index collectedIds then
                        phantomDiv

                      else
                        secDiv "Income ≈"
                    , if pixelCollected pxl.index collectedIds then
                        div
                            [ css [ color green ]
                            , title <|
                                tokenSign
                                    ++ " "
                                    ++ fromWei Ether pxl.ubi
                            ]
                            [ text "√ Collected" ]

                      else
                        priceTag pxl.ubi
                    )

                _ ->
                    ( phantomDiv, priceTag pxl.price )
    in
    div
        [ css
            [ displayFlex
            , alignItems center
            , height (px entryHeight)
            , borderBottom3 (px 1) solid lightbggray
            ]
        ]
        [ div [ css [ displayFlex, marginRight (px 14) ] ] [ box pxl.color ]
        , div [ css [ displayFlex, flexDirection column, width (pct 100) ] ]
            [ div
                [ css [ displayFlex, justifyContent spaceBetween ] ]
                [ div
                    [ css [ displayFlex, property "gap" "4px" ] ]
                    [ coordinates True pxl.index, extraTag ]
                , tradeFlag
                ]
            , value
            ]
        ]
