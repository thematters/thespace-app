module Msg exposing (Msg(..))

import Config.Env.Util exposing (RpcProvider)
import Data
    exposing
        ( ColorChangeDeltaData
        , ColorId
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
import Model.Assets exposing (AssetsSort)


type Msg
    = NoOp
      -- App Modes
    | AppModeChange AppMode
    | PlaybackSpeedChange
    | PlaybackPause
    | PlaybackStart
    | PlaybackTick
    | PlaybackSkipToStart
    | PlaybackSkipToEnd
    | PlaybackSlide String
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
    | SortAssets AssetsSort
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
    | DeltaRecieved (Result Http.Error ColorChangeDeltaData)
