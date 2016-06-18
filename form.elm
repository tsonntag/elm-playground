import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (length)
import Regex exposing (contains, regex)
import List exposing (map, filter,  head)

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }


model : Model
model =
  Model "" "" ""



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update action model =
  case action of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text", placeholder "Name", onInput Name ] []
    , input [ type' "password", placeholder "Password", onInput Password ] []
    , input [ type' "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , viewValidation model
    ]


viewValidation : Model -> Html msg
viewValidation model =
    let
        (color, message) =
            case passwordValidation model.password of
                Just err -> ("red", err)
                Nothing ->
                    if model.password /= model.passwordAgain then
                        ("red", "Passwords do not match!")
                    else
                        ("green", "OK")
    in
        div [ style [("color", color)] ] [ text message ]

{-
ageValidation: String -> Maybe String
ageValidation: age = if contains (regex "^\\d+") age
Just "must be a number"
                     else
-}

validateBool : (String -> Bool) -> String -> MayBe String
validateBool pred, msg, val
    if pred val then
        Nothing
    else
        Just msg

validateRegex : String -> String -> String -> Maybe String
validateRegex regex_s, msg, val = validateBool contains (regex regex_s), msg, val

validateLength : (Int -> Bool), String, String -> Maybe String
validateLength cmp, msg, val =
    let
        pred : String -> Bool
        pred val = cmp (length val)
    in
        validateBool pred, msg, val

validateUpper = validateRegex "[A-Z]+" "must contain upper case characters"
validateLower = validateRegex "[a-z]+" "must contain lower case characters"
validateInt = validateRegex "\\d+" "must contain number"
validateMax max, val = validateLength (>=)

passwordValidation : String -> Maybe String
passwordValidation pw =
    let
        contains_ (r, expected) = ((contains (regex r)) >> not, "must contain " ++ expected)
        contains = map contains_
                  [("\\d", "digits"), ("[a-z]+", "lower case characters"), ("[A-Z]+", "upper case characters")]
        too_short = (length >> (>) 4, "too short")
        preds = too_short :: missing
    in
        case head (filter (fst >> (|>) pw) preds) of
            Nothing -> Nothing
            Just (_, err) -> Just ("Password " ++ err)
