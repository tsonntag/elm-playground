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
passwordValidation : String -> String -> Maybe String
passwordValidation what pw =
    if length pw < 4 then
        Just (what ++ " too short")
    else
        let
            missing (r, expected) = if contains (regex r) pw then
                                        Nothing
                                    else
                                        Just (what ++ " must contain " ++ expected)
        in
            filterMap missing
                [("\\d", "digits"), ("[a-z]+", "lower case characters"), ("[A-Z]+", "upper case characters")]
                    |> head
-}


passwordValidation : String -> Maybe String
passwordValidation pw =
    let
        contains_fn (r, expected) = ((contains (regex r)) >> not, "must contain " ++ expected)
        missing = map contains_fn
                  [("\\d", "digits"), ("[a-z]+", "lower case characters"), ("[A-Z]+", "upper case characters")]
        preds = missing ++ [(length >> (>) 4, "too short")]
    in
        case head (filter (fst >> (|>) pw) preds) of
            Nothing -> Nothing
            Just (_, err) -> Just ("Password " ++ err)
