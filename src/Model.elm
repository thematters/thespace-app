module Model exposing (..)

import Array exposing (Array)
import Config
    exposing
        ( minPrice
        , minZoom
        , miniMapHeight
        , sidebarWidth
        )
import Data exposing (..)
import Dict exposing (Dict)
import InfiniteList
import Model.Assets as A


type alias Flags =
    { winW : Int
    , winH : Int
    , centerCell : Maybe Cell
    , zoom : Maybe Int
    }


initModel : Model
initModel =
    { mode = RealtimeLoading

    --winSize init to (1, 1) avoiding NaN/Infinity bullshit
    , winSize = ( 1, 1 )
    , miniMapMode = CollapsedMiniMap
    , canvas = { dx = 0, dy = 0, zoom = minZoom }
    , pagePos = { x = 0, y = 0 }

    -- cellPos init to (-1,-1) to not display anything in minimap after init
    , cellPos = { x = -1, y = -1 }
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
    , notif = Just LoadingNotif
    , colorHistory = Array.empty
    , queue = Dict.empty
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

    -- Playback Color History
    , colorHistory : Array ColorChange

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
    }


type AppMode
    = RealtimeLoading
    | Realtime
    | PlaybackLoading
    | Playback PlaybackConfig


type alias PlaybackConfig =
    { from : Int
    , to : Int
    , shareFrom : Int
    , shareTo : Int
    , current : Int
    , status : PlaybackStatus
    , speed : PlaybackSpeed
    }


type PlaybackStatus
    = PlaybackStarted
    | PlaybackPaused


type PlaybackSpeed
    = OneX
    | TwoX
    | FourX


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
    | MSLatestColorLoaded (List (Maybe ColorEvent))
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
    = ErrorNotif String
    | LoadingNotif


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


initPlaybackConfig : BlockNumber -> BlockNumber -> PlaybackConfig
initPlaybackConfig from to =
    { from = from
    , to = to
    , shareFrom = from
    , shareTo = to
    , speed = OneX
    , current = 0
    , status = PlaybackPaused
    }


playbackSpeedToString : PlaybackSpeed -> String
playbackSpeedToString spd =
    case spd of
        OneX ->
            "1X"

        TwoX ->
            "2X"

        FourX ->
            "4X"


nextPlaybackSpeed : PlaybackSpeed -> PlaybackSpeed
nextPlaybackSpeed spd =
    case spd of
        OneX ->
            TwoX

        TwoX ->
            FourX

        FourX ->
            OneX


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
