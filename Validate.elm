module Validate exposing (validate, validateRegex, validateLength, validateUpperCase, validateLowerCase, validateInt)
{-|
  Validations

@docs validate, validateRegex, validateLength, validateUpperCase, validateLowerCase, validateInt
-}

import String as S exposing (length)
import Regex as R exposing (contains, regex)
{-
import List as L exposing (map, filter,  head)
-}

{-| V
-}
validate : (String -> Bool) -> String -> String -> Maybe String
validate pred msg val =
    if pred val then
        Nothing
    else
        Just msg
{-| V
-}
validateRegex : String -> String -> String -> Maybe String
validateRegex regex_s msg val =
    let
        pred: String -> Bool
        pred val = R.contains (R.regex regex_s) val
    in
        validate pred msg val
{-| V
-}
validateLength : Int -> String -> String -> Maybe String
validateLength len msg val =
    let
        pred : String -> Bool
        pred val = (S.length val) == len
    in
        validate pred msg val
{-| V
-}
validateUpperCase : String -> Maybe String
validateUpperCase = validateRegex "[A-Z]+" "must contain upper case characters"

{-| V
-}
validateLowerCase : String -> Maybe String
validateLowerCase = validateRegex "[a-z]+" "must contain lower case characters"

{-| V
-}
validateInt : String -> Maybe String
validateInt = validateRegex "\\d+" "must contain number"
