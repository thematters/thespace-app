module Model exposing (..)

import Array exposing (Array)
import Config exposing (minPrice, minZoom, miniMapHeight, sidebarWidth)
import Contract.Space exposing (ColorEvent, Pixel, TreasuryShare)
import Data exposing (..)
import Dict exposing (Dict)
import InfiniteList
import Model.Assets as A
import Model.Playback as PB


type alias Flags =
    { winW : Int
    , winH : Int
    , centerCell : Maybe Cell
    , zoom : Maybe Int
    }


type alias SubId =
    Int


initModel : Model
initModel =
    { mode = Loading

    --winSize init to (1, 1) avoiding NaN/Infinity bullshit
    , winSize = ( 1, 1 )
    , miniMapMode = CollapsedMiniMap
    , canvas = { dx = 0, dy = 0, zoom = minZoom }
    , pagePos = { x = 0, y = 0 }

    -- cellPos init to (-1,-1) to not display anything in minimap after init
    , cellPos = { x = -1, y = -1 }
    , pinch = Nothing
    , dragging = NotDragging
    , cellModalMode = HideTaxUbi
    , selectCell = NoCell
    , input =
        { newColorId = 1
        , newPrice = minPrice
        , quotePrice = minPrice
        }
    , mapStatus = MSLoading
    , wallet = DetectingWallet
    , watchIds = initWatchIds
    , blockNumber = Nothing
    , taxInfo =
        { taxRate = Nothing
        , treasuryShare = Nothing
        , mintTax = Nothing
        }
    , sidebarMode = ( CollapsedSidebar, ActLogs )
    , sidebarInfLists =
        { actsInfList = InfiniteList.init
        , assetsInfList = InfiniteList.init
        }
    , acts = []
    , assets = A.AssetsNotLoaded
    , notif = Just SplashNotif
    , queue = Dict.empty
    , playback = PB.init
    }


type alias Model =
    { -- Realtime or playback
      mode : AppMode

    -- Window sizes
    , winSize : Size

    -- MiniMap Mode
    , miniMapMode : MiniMapMode

    -- Actual image is managed by port, this is only the Transformation
    , canvas : Transform

    -- Current mouse pointing position
    , pagePos : Position

    -- Current pointing transformed canvas pixel position
    , cellPos : Cell

    -- Current pinch distance
    , pinch : Maybe Float

    -- Current dragging info
    , dragging : Dragging

    -- Current Tax Rate / Mint Tax
    , taxInfo : TaxInfo

    -- Current Chain Head
    , blockNumber : Maybe BlockNumber

    -- Subscription Ids for Events
    , watchIds : WatchIds

    -- Wallet
    , wallet : WalletInfo

    -- Init Map
    , mapStatus : MapStatus

    -- Current Selected Cell
    , selectCell : SelectCell

    -- Cell Modal Model
    , cellModalMode : CellModalMode

    -- User Inputs
    , input : UserInput

    -- Request Send Cells Queue
    , queue : Dict Index ColorId

    -- Sidebar Mode
    , sidebarMode : SidebarMode

    -- Sidebar Infinite Lists
    , sidebarInfLists : SidebarInfLists

    -- Acts
    , acts : List Activity

    -- Assets
    , assets : A.Assets

    -- Notification
    , notif : Maybe Notification

    -- Playback
    , playback : PB.Playback
    }


type AppMode
    = Loading
    | Realtime
    | Playback


type MiniMapMode
    = BirdeyeMiniMap
    | CollapsedMiniMap


type Dragging
    = NotDragging
    | MapDragging Position
    | MiniMapDragging Position


type alias SidebarMode =
    ( SidebarUIMode, SidebarInfoType )


type alias SidebarInfLists =
    { actsInfList : InfiniteList.Model
    , assetsInfList : InfiniteList.Model
    }


type MapStatus
    = MSLoading
    | MSSnapshotInited
    | MSLatestColorLoaded (List ColorEvent)
    | MSInited


type alias WatchIds =
    { newHeads : Maybe SubId
    , color : Maybe SubId
    , price : Maybe SubId
    , transfer : Maybe SubId
    , tax : Maybe SubId
    , ubi : Maybe SubId
    , default : Maybe SubId
    }


type alias TaxInfo =
    { taxRate : Maybe TaxRate
    , treasuryShare : Maybe TreasuryShare
    , mintTax : Maybe Price
    }


type SidebarUIMode
    = CollapsedSidebar
    | ExpandedSidebar


type SidebarInfoType
    = ActLogs
    | AssetsManager


type CellModalMode
    = HideTaxUbi
    | ShowTaxUbi


type alias UserInput =
    { newColorId : ColorId
    , newPrice : Price
    , quotePrice : Price
    }


type SelectCell
    = NoCell
    | LoadingCell Index
    | LoadedCell ( Pixel, PixelOpStep, Maybe RpcErrorData )


type PixelOpStep
    = PickNewColor
    | SetNewPrice
    | WaitForWallet
    | WaitForChain


type Notification
    = SplashNotif
    | ConnectionLostNotif
    | ReconnectingNotif


initWatchIds : WatchIds
initWatchIds =
    { newHeads = Nothing
    , color = Nothing
    , price = Nothing
    , transfer = Nothing
    , tax = Nothing
    , ubi = Nothing
    , default = Nothing
    }


responsiveMiniMapMode : Size -> MiniMapMode
responsiveMiniMapMode ( _, winH ) =
    if toFloat winH < 4 * miniMapHeight then
        CollapsedMiniMap

    else
        BirdeyeMiniMap


responsiveSiebarMode : Size -> SidebarMode -> SidebarMode
responsiveSiebarMode ( winW, _ ) ( _, infoType ) =
    if toFloat winW < 4 * sidebarWidth then
        ( CollapsedSidebar, infoType )

    else
        ( ExpandedSidebar, infoType )
