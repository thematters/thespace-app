{-
   Adapted from elm-ethereum-generator v4.0.0 generated file.

   https://github.com/cmditch/elm-ethereum-generator
-}


module Contract.ERC20 exposing (allowance, approve, balanceOf)

import BigInt exposing (BigInt)
import Eth.Abi.Decode as AD
import Eth.Abi.Encode as AE
import Eth.Types exposing (Address, Call)


allowance : Address -> Address -> Address -> Call BigInt
allowance contractAddress owner_ spender_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data =
        Just <|
            AE.functionCall "dd62ed3e"
                [ AE.address owner_, AE.address spender_ ]
    , nonce = Nothing
    , decoder = AD.toElmDecoder AD.uint
    }


approve : Address -> Address -> Address -> BigInt -> Call Bool
approve contractAddress owner_ spender_ amount_ =
    { to = Just contractAddress
    , from = Just owner_
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data =
        Just <|
            AE.functionCall "095ea7b3"
                [ AE.address spender_, AE.uint amount_ ]
    , nonce = Nothing
    , decoder = AD.toElmDecoder AD.bool
    }


balanceOf : Address -> Address -> Call BigInt
balanceOf contractAddress account_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AE.functionCall "70a08231" [ AE.address account_ ]
    , nonce = Nothing
    , decoder = AD.toElmDecoder AD.uint
    }
