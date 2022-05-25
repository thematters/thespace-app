module View.Cell exposing (viewSelectCell)

import BigInt
import Config
    exposing
        ( cellModalHeight
        , cellModalWidth
        , cellModalzIndex
        , colorHexs
        , highlightZoom
        , inputPriceDecimalDigits
        , lightColor
        , maxPrice
        , minPrice
        , toPolyscanAddressUrl
        , toPolyscanTokenUrl
        , tokenSign
        , zeroPrice
        )
import Css exposing (..)
import Data
    exposing
        ( Cell
        , ColorId
        , Pixel
        , Position
        , Price
        , RpcErrorKind(..)
        , Size
        , TaxRate
        , Transform
        , WalletInfo(..)
        , ZoomLevel
        , accTaxNumBlocks
        , cellToPos
        , fromWei
        , indexToCell
        , notOnChain
        , safePrice
        , sizeToFloatSize
        , unsafeColorIdToHexColor
        , walletIsAddress
        )
import Data.Icon as Icons
import Eth.Defaults exposing (zeroAddress)
import Eth.Units exposing (EthUnit(..), bigIntToWei, toWei)
import Html.Styled exposing (Html, button, div, input, text)
import Html.Styled.Attributes exposing (css, href, title, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Model
    exposing
        ( CellModalMode(..)
        , PixelOpStep(..)
        , SelectCell(..)
        , TaxInfo
        , UserInput
        )
import Msg exposing (Msg(..))
import Round
import View.Common
    exposing
        ( addressTag
        , bggray
        , bigText
        , bigTextDiv
        , bigTextSize
        , buttonDisabledStyle
        , buttonInvertStyle
        , buttonStyle
        , connectMetaMaskLink
        , coords
        , depositLink
        , getTokenLink
        , gray
        , grayDiv
        , green
        , greenAddressTag
        , highlightColor1Str
        , iconBlack
        , iconLight
        , iconNormal
        , iconWhite
        , installMetaMaskLink
        , lightgray
        , lightgrayStr
        , lockedMataMaskBtn
        , modalBackgroundColor
        , modalBoxShadow
        , modalEdge
        , normalTextSize
        , opLinkStyle
        , orangeDiv
        , phantomDiv
        , placeholderDiv
        , priceTag
        , redDiv
        , secDiv
        , secondary
        , smallText
        , spinner
        , switchNetworkLink
        , syncPriceLink
        , textDiv
        , white
        )


opAreaHeight : Float
opAreaHeight =
    130


positionStyle : Size -> ZoomLevel -> Position -> List Style
positionStyle winSize zoom pos =
    let
        ( windowW, windowH ) =
            winSize |> sizeToFloatSize

        ( modalW, modalH ) =
            ( cellModalWidth, cellModalHeight )

        square =
            px 0

        round =
            px modalEdge
    in
    case
        ( windowW - pos.x >= modalW, windowH - pos.y >= modalH )
    of
        ( True, True ) ->
            [ left (px (pos.x + zoom))
            , top (px (pos.y + zoom))
            , borderRadius4 square round round round
            ]

        ( True, False ) ->
            [ left (px (pos.x + zoom))
            , top (px (pos.y - modalH))
            , borderRadius4 round round round square
            ]

        ( False, True ) ->
            [ left (px (pos.x - modalW))
            , top (px (pos.y + zoom))
            , borderRadius4 round square round round
            ]

        ( False, False ) ->
            [ left (px (pos.x - modalW))
            , top (px (pos.y - modalH))
            , borderRadius4 round round square round
            ]


modalBasicStyle : Size -> Transform -> Cell -> List Style
modalBasicStyle winSize canvas cell =
    let
        baseStyle =
            [ position absolute
            , zIndex (int cellModalzIndex)
            , backgroundColor modalBackgroundColor
            , modalBoxShadow
            , width (px <| cellModalWidth - 2 * modalEdge)
            , height (px <| cellModalHeight - 2 * modalEdge)
            , padding (px modalEdge)
            ]

        pos =
            positionStyle winSize canvas.zoom (cellToPos canvas cell)
    in
    baseStyle ++ pos


maskStyle : List Style
maskStyle =
    [ zIndex (int <| cellModalzIndex + 1)
    , backgroundColor <| Css.rgba 256 256 256 0.8
    , position absolute
    , left (px 0)
    , right (px 0)
    , top (px 0)
    , displayFlex
    , justifyContent spaceAround
    , alignItems center
    , color gray
    ]


modalMask : List Style -> Mask -> Html msg
modalMask style_ mask =
    let
        mask_ c =
            div [ css <| style_ ++ maskStyle ]
                [ div [ css [ backgroundColor white, fontSize bigText ] ]
                    [ c ]
                ]
    in
    case mask of
        NoMask ->
            phantomDiv

        NormalMask s ->
            mask_ <| textDiv s

        ErrorMask s ->
            mask_ <| redDiv s

        UnderPricedMask ->
            mask_ <| redDiv "Tx Under Priced."


type Mask
    = NoMask
    | NormalMask String
    | UnderPricedMask
    | ErrorMask String


type alias ViewParams =
    { mode : CellModalMode
    , pixel : Pixel
    , modalStyle : List Style
    , mask : Mask
    , wallet : WalletInfo
    , newColorId : ColorId
    , newPrice : Price
    , quotePrice : Price
    , step : PixelOpStep
    , taxRate : Maybe TaxRate
    , mintTax : Maybe Price
    }


viewSelectCell : CellModalMode -> Size -> Transform -> SelectCell -> WalletInfo -> TaxInfo -> UserInput -> Html Msg
viewSelectCell mode winSize canvas scell wallet { taxRate, mintTax } { newColorId, newPrice, quotePrice } =
    let
        modalStyle cell =
            modalBasicStyle winSize canvas cell

        viewParams pxl mask step =
            { pixel = pxl
            , modalStyle = modalStyle <| indexToCell pxl.index
            , mask = mask
            , step = step
            , mode = mode
            , wallet = wallet
            , newColorId = newColorId
            , newPrice = newPrice
            , quotePrice = quotePrice
            , taxRate = taxRate
            , mintTax = mintTax
            }
    in
    if canvas.zoom < highlightZoom then
        phantomDiv

    else
        case scell of
            NoCell ->
                phantomDiv

            LoadingCell idx ->
                loadingView <| modalStyle <| indexToCell idx

            LoadedCell ( pixel, PickNewColor, _ ) ->
                let
                    params =
                        viewParams pixel NoMask PickNewColor
                in
                if walletIsAddress wallet pixel.owner then
                    ownerView params

                else
                    pickColorView params

            LoadedCell ( pixel, SetNewPrice, _ ) ->
                let
                    params =
                        viewParams pixel NoMask SetNewPrice
                in
                if walletIsAddress wallet pixel.owner then
                    ownerView params

                else
                    setPriceView params

            LoadedCell ( pixel, WaitForWallet, Nothing ) ->
                let
                    mask =
                        NormalMask "Please confirm in your wallet."

                    params =
                        viewParams pixel mask SetNewPrice
                in
                if walletIsAddress wallet pixel.owner then
                    ownerView params

                else
                    setPriceView params

            LoadedCell ( pixel, WaitForWallet, Just _ ) ->
                let
                    mask =
                        --case err.kind of
                        --    RpcUnderPricedError _ ->
                        --        UnderPricedMask
                        --    RpcUnknownError ->
                        ErrorMask <|
                            "MetaMask says something is wrong, "
                                ++ "open it for details."

                    params =
                        viewParams pixel mask SetNewPrice
                in
                if walletIsAddress wallet pixel.owner then
                    ownerView params

                else
                    setPriceView params

            LoadedCell ( { index }, WaitForChain, _ ) ->
                waitingConfirm <| modalStyle <| indexToCell index


loadingStyle : List Style
loadingStyle =
    [ displayFlex
    , justifyContent spaceAround
    , alignItems center
    , textAlign center
    ]


loadingView : List Style -> Html msg
loadingView modalStyle =
    div [ css <| fontSize bigText :: modalStyle ++ loadingStyle ] [ spin ]


waitingConfirm : List Style -> Html Msg
waitingConfirm modalStyle =
    div [ css <| modalStyle ++ loadingStyle ]
        [ div []
            [ bigTextDiv <| grayDiv "Request send."
            , grayDiv "Confirmation may take a few seconds."
            , grayDiv "Just keep playing :)"
            , div [ css [ marginTop (px 14) ] ] [ spin ]
            ]
        ]


ownerView : ViewParams -> Html Msg
ownerView { modalStyle, mask, wallet, pixel, newColorId, newPrice, quotePrice, step, taxRate, mintTax } =
    let
        style_ =
            modalStyle
                ++ [ displayFlex
                   , flexDirection column
                   , justifyContent spaceBetween
                   ]
    in
    div [ css style_ ]
        [ modalMask modalStyle mask
        , pixelInfo wallet pixel
        , colorPicker newColorId
        , div []
            [ keepCurrentPrice pixel.price newPrice
            , priceInput newPrice
            , estimatedTax taxRate newPrice
            ]
        , taxIncomeInfoAndOps wallet pixel
        , pixelButton mintTax wallet pixel newColorId newPrice quotePrice step
        ]


pickColorView : ViewParams -> Html Msg
pickColorView { modalStyle, mode, wallet, pixel, newColorId, newPrice, quotePrice, step, mintTax } =
    let
        style_ =
            modalStyle
                ++ [ displayFlex
                   , flexDirection column
                   , justifyContent spaceBetween
                   ]

        opArea =
            div
                [ css [ height (px opAreaHeight) ] ]
                [ opLabel "Pick New Color"
                , div [ css [ marginTop (px 14) ] ] [ colorPicker newColorId ]
                ]
    in
    div
        [ css style_ ]
    <|
        [ pixelInfo wallet pixel
        , priceInfo mode PickNewColor wallet pixel quotePrice
        , taxIncomeInfo mode wallet pixel
        , opArea
        , pixelButton mintTax wallet pixel newColorId newPrice quotePrice step
        ]


setPriceView : ViewParams -> Html Msg
setPriceView { modalStyle, mask, mode, wallet, pixel, newColorId, newPrice, quotePrice, step, taxRate, mintTax } =
    let
        style_ =
            modalStyle
                ++ [ displayFlex
                   , flexDirection column
                   , justifyContent spaceBetween
                   ]

        tickedColor =
            let
                boxSize =
                    normalTextSize * 2
            in
            div
                [ css
                    [ displayFlex, alignItems end, property "gap" "8px" ]
                , onClick PixelOpBack
                ]
                [ div [ css [ cursor pointer ] ] [ secDiv "Change" ]
                , tickedBox boxSize newColorId
                ]

        labelAndKeepCurrentPrice =
            div
                [ css
                    [ displayFlex
                    , flexDirection column
                    , justifyContent spaceBetween
                    ]
                ]
                [ opLabel "Set New Price"
                , keepCurrentPrice pixel.price newPrice
                ]

        opArea =
            div [ css [ height (px opAreaHeight) ] ]
                [ div
                    [ css [ displayFlex, justifyContent spaceBetween ] ]
                    [ labelAndKeepCurrentPrice, tickedColor ]
                , priceInput newPrice
                , estimatedTax taxRate newPrice
                ]
    in
    div
        [ css style_ ]
    <|
        [ modalMask modalStyle mask
        , pixelInfo wallet pixel
        , priceInfo mode SetNewPrice wallet pixel quotePrice
        , taxIncomeInfo mode wallet pixel
        , opArea
        , pixelButton mintTax wallet pixel newColorId newPrice quotePrice step
        ]


spin : Html msg
spin =
    spinner (normalTextSize * 2) lightgrayStr


polyscanLink : String -> Html Msg -> Html Msg
polyscanLink href_ e =
    Html.Styled.a
        [ css [ textDecoration none, color unset, outline none ]
        , href href_
        , Html.Styled.Attributes.target "_blank"
        ]
        [ div
            [ css [ displayFlex, alignItems center, property "gap" "2px" ] ]
            [ e, Icons.polygon ]
        ]


pixelInfo : WalletInfo -> Pixel -> Html Msg
pixelInfo wallet pixel =
    let
        style_ =
            [ displayFlex, alignItems start, justifyContent spaceBetween ]

        coods =
            polyscanLink
                (toPolyscanTokenUrl pixel.index)
                (coords grayDiv True pixel.index)
    in
    div [ css style_ ]
        [ div [ css [ displayFlex, alignItems center, property "gap" "8px" ] ]
            [ coods
            , grayDiv "owner"
            , ownerInfo wallet pixel
            ]
        , close
        ]


ownerInfo : WalletInfo -> Pixel -> Html Msg
ownerInfo wallet pixel =
    let
        polyscanUrl =
            toPolyscanAddressUrl pixel.owner

        addrTag =
            case wallet of
                Wallet { address } ->
                    if address == Just pixel.owner then
                        greenAddressTag pixel.owner

                    else
                        addressTag pixel.owner

                _ ->
                    addressTag pixel.owner
    in
    polyscanLink polyscanUrl addrTag


close : Html Msg
close =
    div [ css [ cursor pointer ], title "Close", onClick <| ClearSelectedCell ]
        [ iconLight Icons.close ]


priceInfo : CellModalMode -> PixelOpStep -> WalletInfo -> Pixel -> Price -> Html Msg
priceInfo mode step wallet pixel quotePrice =
    div []
        [ div
            [ css
                [ displayFlex
                , alignItems center
                , fontSize (px <| bigTextSize * 1.5)
                , whiteSpace noWrap
                ]
            ]
            [ priceTag pixel.price ]
        , taxIncomeToggle mode step wallet pixel quotePrice
        ]


taxIncomeToggle : CellModalMode -> PixelOpStep -> WalletInfo -> Pixel -> Price -> Html Msg
taxIncomeToggle mode step wallet pixel quotePrice =
    let
        switchTo =
            case mode of
                ShowTaxUbi ->
                    HideTaxUbi

                HideTaxUbi ->
                    ShowTaxUbi

        icon =
            div [ css [ displayFlex ] ]
                [ case mode of
                    ShowTaxUbi ->
                        iconLight Icons.chevronUp

                    HideTaxUbi ->
                        iconNormal Icons.chevronDown
                ]

        name =
            "Tax/Income"

        label =
            case mode of
                ShowTaxUbi ->
                    grayDiv name

                HideTaxUbi ->
                    secDiv name

        priceChanged =
            if pixel.price == quotePrice || step == PickNewColor then
                placeholderDiv

            else
                orangeDiv "Price Changed"

        warning =
            case mode of
                ShowTaxUbi ->
                    phantomDiv

                HideTaxUbi ->
                    riskWarning wallet pixel
    in
    div
        [ css
            [ displayFlex
            , alignItems center
            , justifyContent spaceBetween
            ]
        ]
        [ div
            []
            [ priceChanged
            , warning
            ]
        , div
            [ css [ displayFlex, cursor pointer ]
            , onClick <| SwitchTaxUbiMode switchTo
            ]
            [ icon, label ]
        ]


taxIncomeInfo : CellModalMode -> WalletInfo -> Pixel -> Html Msg
taxIncomeInfo mode wallet pixel =
    case mode of
        HideTaxUbi ->
            phantomDiv

        ShowTaxUbi ->
            taxIncomeInfoAndOps wallet pixel


taxIncomeInfoAndOps : WalletInfo -> Pixel -> Html Msg
taxIncomeInfoAndOps wallet pixel =
    div
        [ css
            [ displayFlex
            , flexDirection column
            , justifyContent spaceBetween
            , height (px 80)
            ]
        ]
        [ taxInfo pixel.tax
        , incomeInfo pixel.ubi
        , taxIncomeOps wallet pixel
        ]


taxInfo : Price -> Html msg
taxInfo =
    taxOrIncomeInfo "Due Tax"


incomeInfo : Price -> Html msg
incomeInfo =
    taxOrIncomeInfo "Acc. Income"


taxOrIncomeInfo : String -> Price -> Html msg
taxOrIncomeInfo label value =
    div [ css [ displayFlex, justifyContent spaceBetween ] ]
        [ secDiv label
        , div [ css [ displayFlex ] ] [ grayDiv "≈", priceTag value ]
        ]


taxIncomeOps : WalletInfo -> Pixel -> Html Msg
taxIncomeOps wallet pixel =
    let
        btnStyle =
            [ displayFlex
            , justifyContent center
            , alignItems center
            , height (px 28)
            , padding4 (px 0) (px 14) (px 0) (px 14)
            , borderRadius (px 12)
            , backgroundColor green
            , textDecoration none
            , focus [ outline none ]
            , color white
            , border3 (px 1) solid green
            , cursor pointer
            , fontSize smallText
            , fontWeight bold
            , marginTop (px 2)
            ]

        btn i s =
            button [ css btnStyle, onClick <| CollectIncome i ] [ text s ]

        whatArethese =
            div [ css [ fontSize smallText ] ]
                [ Html.Styled.a
                    [ css [ textDecoration none ]
                    , Html.Styled.Attributes.target "_blank"
                    , href Config.whatAreTheseNumbersLink
                    ]
                    [ grayDiv "What are these numbers?" ]
                ]

        collect =
            case wallet of
                Wallet _ ->
                    if
                        walletIsAddress wallet pixel.owner
                            && BigInt.gt pixel.ubi minPrice
                    then
                        btn pixel.index "Collect Income"

                    else
                        whatArethese

                _ ->
                    whatArethese
    in
    div
        [ css
            [ displayFlex
            , alignItems start
            , justifyContent spaceBetween
            , alignItems end
            ]
        ]
        [ riskWarning wallet pixel
        , collect
        ]


riskWarning : WalletInfo -> Pixel -> Html msg
riskWarning wallet pixel =
    let
        who =
            let
                s =
                    "Owner "
            in
            case wallet of
                Wallet { address } ->
                    if Just pixel.owner == address then
                        ""

                    else
                        s

                _ ->
                    s

        warning kind =
            div [ css [ displayFlex, alignItems end, property "gap" "4px" ] ]
                [ redDiv "Default Risk"
                , div [ css [ color gray, fontSize smallText ] ]
                    [ text <| who ++ kind ++ " < Tax" ]
                ]

        warningBalance =
            warning "Balance"

        warningAllowance =
            warning "Allowance"

        risky i =
            BigInt.gt pixel.tax i
    in
    case ( pixel.ownerBalance, pixel.ownerAllowance ) of
        ( Just blc, Just alw ) ->
            if risky blc && pixel.owner /= zeroAddress then
                warningBalance

            else if risky alw && pixel.owner /= zeroAddress then
                warningAllowance

            else
                placeholderDiv

        _ ->
            placeholderDiv


priceInput : Price -> Html Msg
priceInput newPrice =
    let
        style_ =
            [ displayFlex
            , flexDirection column
            , margin4 (px 10) (px 0) (px 4) (px 0)
            ]

        inputStyle =
            [ displayFlex
            , width (pct 100)
            , padding (px 10)
            , border3 (px 1) solid lightgray
            , borderRadius (px 4)
            , backgroundColor <| Css.rgba 224 224 224 0.4
            , alignItems center
            , marginBottom (px 4)
            , paddingLeft (px 74)
            , focus [ outline none ]
            ]

        max =
            maxPrice |> fromWei Ether

        min =
            minPrice |> fromWei Ether

        priceValue =
            newPrice |> fromWei Ether

        unit =
            div
                [ css
                    [ position absolute
                    , displayFlex
                    , alignItems center
                    , color gray
                    , margin4 (px -4) (px 0) (px 0) (px 12)
                    ]
                ]
                [ text tokenSign ]

        validateInput =
            let
                keepDecimalDigits =
                    Round.round inputPriceDecimalDigits
                        >> toWei Ether
                        >> Result.map safePrice
                        >> Result.withDefault minPrice
            in
            String.toFloat
                >> Maybe.map keepDecimalDigits
                >> Maybe.withDefault minPrice

        input_ =
            div
                [ css [ displayFlex, alignItems center ] ]
                [ unit
                , input
                    [ type_ "number"
                    , Html.Styled.Attributes.max max
                    , Html.Styled.Attributes.min min
                    , css inputStyle
                    , value priceValue
                    , onInput <| validateInput >> EnterPrice
                    ]
                    []
                ]
    in
    div [ css style_ ] [ input_ ]


keepCurrentPrice : Price -> Price -> Html Msg
keepCurrentPrice oldPrice newPrice =
    if oldPrice /= newPrice then
        div
            [ css [ cursor pointer, color secondary ]
            , onClick <| EnterPrice oldPrice
            ]
            [ secDiv "Keep Current Price" ]

    else
        div [ css [ color lightgray ] ] [ grayDiv "Current Price" ]


estimatedTax : Maybe TaxRate -> Price -> Html msg
estimatedTax taxRate newPrice =
    let
        h =
            20

        estmatedBlockNumPerDay =
            24 * 60 * 60 // 2
    in
    let
        style_ =
            [ displayFlex
            , flexDirection row
            , alignItems start
            , property "gap" "2px"
            , whiteSpace noWrap
            , height (px h)
            ]

        estimat =
            accTaxNumBlocks newPrice estmatedBlockNumPerDay taxRate
    in
    div
        [ css style_ ]
        [ grayDiv "Estimated tax per day ≈", priceTag estimat ]


opLabel : String -> Html msg
opLabel s =
    div
        [ css [ fontSize bigText, color secondary ] ]
        [ text s ]


boxGap : Float
boxGap =
    12


colorPicker : ColorId -> Html Msg
colorPicker tickedColorId =
    let
        boxWidth =
            40

        style_ =
            [ displayFlex, flexDirection column, width (pct 100) ]

        lineStyle =
            [ displayFlex
            , alignItems center
            , justifyContent center
            , pseudoClass "not(:last-child)" [ marginBottom (px boxGap) ]
            ]

        drawBox i =
            if i == tickedColorId then
                tickedBox boxWidth i

            else
                normalBox boxWidth i

        ics =
            colorHexs
                |> List.length
                |> List.range 1
                |> List.map drawBox
    in
    div
        [ css style_ ]
        [ div [ css lineStyle ] (List.take 8 ics)
        , div [ css lineStyle ] (List.drop 8 ics)
        ]


box : Float -> Int -> Bool -> Html Msg
box size i ticked =
    let
        hexc =
            unsafeColorIdToHexColor i

        style_ c =
            let
                boxSize =
                    if lightColor c then
                        size - 2

                    else
                        size

                baseStyle =
                    [ backgroundColor (hex c)
                    , width (px boxSize)
                    , height (px boxSize)
                    , cursor pointer
                    , pseudoClass "not(:last-child)" [ marginRight (px boxGap) ]
                    ]
            in
            if lightColor c then
                border3 (px 1) solid bggray :: baseStyle

            else
                baseStyle

        ticker =
            if ticked then
                [ div
                    [ css
                        [ displayFlex
                        , alignItems end
                        , flexDirection rowReverse
                        , fontSize (px <| size * 0.8)
                        , marginTop (px <| size * 0.2)
                        , color (hex highlightColor1Str)
                        ]
                    ]
                    [ text "√" ]
                ]

            else
                []
    in
    div [ css <| style_ hexc, onClick <| TickColor i ] ticker


normalBox : Float -> Int -> Html Msg
normalBox size i =
    box size i False


tickedBox : Float -> Int -> Html Msg
tickedBox size i =
    box size i True


pixelButton : Maybe Price -> WalletInfo -> Pixel -> ColorId -> Price -> Price -> PixelOpStep -> Html Msg
pixelButton mintTax wallet pixel newColorId newPrice quotePrice step =
    let
        btn disabled invert icon event s =
            let
                btnStyle =
                    if disabled then
                        buttonDisabledStyle

                    else if invert then
                        buttonInvertStyle

                    else
                        buttonStyle

                iconText i t =
                    [ div
                        [ css
                            [ displayFlex
                            , alignItems center
                            , property "gap" "4px"
                            , whiteSpace noWrap
                            ]
                        ]
                        (if disabled then
                            [ iconLight i, text t ]

                         else if invert then
                            [ iconWhite i, text t ]

                         else
                            [ iconBlack i, text t ]
                        )
                    ]
            in
            button
                (case event of
                    Nothing ->
                        [ css btnStyle ]

                    Just evt ->
                        [ css btnStyle, onClick evt ]
                )
                (case icon of
                    Nothing ->
                        [ text s ]

                    Just i ->
                        iconText i s
                )

        priceChanged =
            pixel.price /= quotePrice

        bgt a b =
            BigInt.gt a b

        blt a b =
            BigInt.lt a b

        canSnap pxl blc alw =
            let
                bidderCanAffordMintTax mt =
                    bgt blc mt && bgt alw mt

                ownerCanNotAffordTax oblc oalw =
                    blt oblc pixel.tax || blt oalw pixel.tax
            in
            case ( mintTax, pxl.ownerBalance, pxl.ownerAllowance ) of
                ( Just mt, Just owrblc, Just owralw ) ->
                    if
                        ownerCanNotAffordTax owrblc owralw
                            && bidderCanAffordMintTax mt
                            && pxl.owner
                            /= zeroAddress
                    then
                        True

                    else
                        False

                _ ->
                    False

        opBtns walt =
            case walt of
                DetectingWallet ->
                    [ checkingBtn ]

                NoWallet ->
                    [ metaMaskNotInstalledBtn, installMetaMaskLink ]

                LockedWallet ->
                    [ lockedMataMaskBtn ]

                Wallet { chainId, address, allowance, balance } ->
                    if notOnChain chainId then
                        [ notOnPolygonBtn, switchNetworkLink ]

                    else
                        case ( address, balance, allowance ) of
                            ( Nothing, _, _ ) ->
                                [ notConnectBtn, connectMetaMaskLink ]

                            ( _, Nothing, _ ) ->
                                [ checkingBtn ]

                            ( _, _, Nothing ) ->
                                [ checkingBtn ]

                            ( Just _, Just blc, Just alw ) ->
                                if blc == zeroPrice then
                                    [ noTokenBtn, getTokenLink ]

                                else if alw == zeroPrice then
                                    [ notDepositedBtn, depositLink ]

                                else if canSnap pixel blc alw then
                                    [ snapBtn ]

                                else if BigInt.lt blc pixel.price then
                                    if priceChanged then
                                        [ balanceNotEnoughBtn, syncPriceLink ]

                                    else if blc == zeroPrice then
                                        [ balanceNotEnoughBtn, getTokenLink ]

                                    else
                                        [ balanceNotEnoughBtn ]

                                else if BigInt.lt alw pixel.price then
                                    if priceChanged then
                                        [ allowanceNotEnoughBtn, depositLink ]

                                    else
                                        [ allowanceNotEnoughBtn, syncPriceLink ]

                                else if priceChanged then
                                    [ buyBtn, syncPriceLink ]

                                else
                                    [ buyBtn ]

        updatePixel =
            BidPixel ( zeroPrice, newPrice, newColorId )

        snapPixel mt =
            BidPixel ( mt, newPrice, newColorId )

        buyPixel =
            BidPixel ( quotePrice, newPrice, newColorId )

        setPriceBtn =
            let
                icon =
                    Just Icons.arrowRight
            in
            btn False False icon (Just PixelOpForward) "Set New Price"

        checkingBtn =
            btn True False Nothing Nothing "Checking Wallet Status"

        metaMaskNotInstalledBtn =
            btn True False Nothing Nothing "MetaMask Not Installed"

        notOnPolygonBtn =
            btn True False Nothing Nothing "MetaMask Not on Polygon"

        notConnectBtn =
            btn True False Nothing Nothing "MetaMask Not Connected"

        nothingChangedBtn =
            btn True False Nothing Nothing "Pick Different Color/Pirce to Update"

        updateBtn =
            btn False True Nothing (Just updatePixel) "Update Color/Price"

        noTokenBtn =
            btn True False Nothing Nothing <| "No " ++ tokenSign

        notDepositedBtn =
            btn True False Nothing Nothing <| tokenSign ++ " Not Approved"

        notEnoughStr =
            "Not Enough " ++ tokenSign

        balanceNotEnoughBtn =
            btn True False Nothing Nothing notEnoughStr

        allowanceNotEnoughBtn =
            btn True False Nothing Nothing notEnoughStr

        snapBtn =
            let
                mt =
                    mintTax |> Maybe.withDefault zeroPrice

                s =
                    "Default Risk Pixel. Try Snap at ONLY "
                        ++ fromWei Ether mt
                        ++ "?"
            in
            btn False True Nothing (Just <| snapPixel mt) s

        buyBtn =
            let
                icon =
                    Just Icons.shoppingCart

                s =
                    "Pay " ++ fromWei Ether quotePrice ++ " & Buy!"
            in
            btn False True icon (Just buyPixel) s

        back =
            div
                [ css opLinkStyle, onClick PixelOpBack ]
                [ iconBlack Icons.arrowLeft ]

        btns =
            back :: opBtns wallet
    in
    if step == PickNewColor then
        if walletIsAddress wallet pixel.owner then
            if pixel.color == newColorId && pixel.price == newPrice then
                nothingChangedBtn

            else
                updateBtn

        else
            setPriceBtn

    else
        div
            [ css
                [ displayFlex
                , alignItems end
                , justifyContent spaceBetween
                , property "gap" "14px"
                ]
            ]
            btns
