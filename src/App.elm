module App exposing (main)

import Markdown.AST exposing (..)
import Markdown.Parser as Parser exposing (..)
import Html exposing (Html, program)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Parser as P
import Markdown.Renderer as Renderer


type alias Model =
    { input : String
    , output : Result P.Error (List Block)
    }


initialModel : Model
initialModel =
    { input = "", output = Parser.parse "" }


type Msg
    = Input String


update : Msg -> Model -> ( Model, Cmd Msg )
update (Input newInput) model =
    { input = newInput
    , output = Parser.parse newInput
    }
        ! []


view : Model -> Html Msg
view { input, output } =
    Html.div [] <|
        [ renderInput input
        , Html.hr [] []
        ]
            ++ renderOutput output


renderOutput : Result P.Error (List Block) -> List (Html a)
renderOutput output =
    case output of
        Err error ->
            [ Html.pre [] [ Html.text <| toString error ] ]

        Ok doc ->
            [ renderTree doc
            , Html.div [] <| Renderer.render doc
            ]


renderInput : String -> Html Msg
renderInput input =
    Html.textarea [ value input, onInput Input ] []


renderTree : List Block -> Html a
renderTree blocks =
    Html.pre []
        [ Html.text <| String.join "\n" <| List.map toString blocks
        ]


main : Program Never Model Msg
main =
    program
        { init = initialModel ! []
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
