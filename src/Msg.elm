module Msg exposing (Msg(..))

import Config.Env.Util exposing (RpcProvider)
import Data
    exposing
        ( ColorId
        , Index
        , Position
        , Price
        , Size
        , WalletInfo
        )
import Http
import InfiniteList
import Json.Encode exposing (Value)
import Model
    exposing
        ( AppMode
        , CellModalMode(..)
        , MiniMapMode
        , SidebarInfoType(..)
        , SidebarUIMode(..)
        )
import Model.Assets exposing (Rank)
import Model.Playback exposing (DeltaData)


type Msg
    = NoOp
      -- App Modes
    | AppModeChange AppMode
    | PlaybackSnapshotReady
    | PlaybackRewindTimeline (List String)
    | PlaybackPlay
    | PlaybackPause
    | PlaybackSlide Int
    | PlaybackTicked
    | PlaybackCircleSpeed
      -- MiniMap Modes
    | MiniMapModeChange MiniMapMode
      -- Map / MiniMap Ops
    | WindowResize Size
    | MapMouseDown Position
    | MapTouchDown Position
    | MapPinchDown Float
    | MapPinchChange Float
    | MiniMapMouseDown Position
    | MouseUp Position
    | MouseMove Position
    | TouchMove Position
    | ZoomIn
    | ZoomOut
    | ZoomInCenter
    | ZoomOutCenter
    | ZoomReset
      -- Sidebar Ops
    | SidebarModeSwitch SidebarUIMode
    | SidebarInfoSwitch SidebarInfoType
    | ScrollActivity InfiniteList.Model
    | ScrollAssets InfiniteList.Model
    | SortAssets Rank
    | LoadAssets
    | RefreshAssets
      -- Cell Ops
    | SelectCell ( Index, Bool ) -- (CellIndex, CenterToCellOrNot)
    | ClearSelectedCell
    | SwitchTaxUbiMode CellModalMode
    | TickColor ColorId
    | EnterPrice Price
    | PixelOpForward
    | PixelOpBack
    | SyncQuotePrice
    | BidPixel ( Price, Price, ColorId ) -- (bidPrice, newPrice, newColor)
    | CollectIncome Index
      -- Wallet
    | ConnectMetaMask
    | SwitchWalletNetwork RpcProvider
    | WalletInfoChanged WalletInfo
    | WalletLocked
    | RequestApproveAllBalance
    | IncomeCollected
    | IncomeCollectFailed
    | AllowanceApproved Price
    | AllowanceRejected
    | TxConfirmed Index
    | TxUnderPriced Index
    | TxRejected Index
      -- Rpc
    | RpcSocketOpened
    | MapSnapshotInited
    | RpcMessageRecieved Value
    | RpcSocketClosed
    | RpcSocketReconnecting
    | RpcSocketReconnected
    | ReInitApp
    | DeltaRecieved (Result Http.Error DeltaData)
