module Contract.Util exposing
    ( bigIntAsIntDecoder
    , bigIntToInt
    , unsafeBigIntToInt
    )

import BigInt exposing (BigInt)
import Eth.Decode as ED
import Json.Decode as D


bigIntAsIntDecoder : D.Decoder Int
bigIntAsIntDecoder =
    let
        f bi =
            case bigIntToInt bi of
                Nothing ->
                    D.fail "decode BigInt as Int fail"

                Just i ->
                    D.succeed i
    in
    ED.bigInt |> D.andThen f


bigIntToInt : BigInt -> Maybe Int
bigIntToInt =
    -- yeah, yeah, I know this is hacky...
    -- but Web3 is hackier...
    BigInt.toString >> String.toInt


unsafeBigIntToInt : BigInt -> Int
unsafeBigIntToInt =
    bigIntToInt >> Maybe.withDefault 0
