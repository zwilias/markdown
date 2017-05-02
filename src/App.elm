module App exposing (main)

import Markdown.Parser.BlockStructure.Types exposing (..)
import Markdown.Parser.BlockStructure as BlockStructure
import Markdown.Parser as Parser exposing (..)
import Html exposing (Html, program)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)


type alias Model =
    { input : String
    , output : BlockStructure.ParseResult
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


renderOutput : BlockStructure.ParseResult -> List (Html a)
renderOutput output =
    case output of
        Err error ->
            [ Html.pre [] [ Html.text <| toString error ] ]

        Ok doc ->
            [ renderTree doc ]


renderInput : String -> Html Msg
renderInput input =
    Html.textarea [ value input, onInput Input ] []


renderHtml : List (Html a) -> Html a
renderHtml =
    Html.div []


renderTree : ( List Block, b ) -> Html a
renderTree ( blocks, _ ) =
    Html.pre []
        [ Html.text <| toString blocks
        ]


main : Program Never Model Msg
main =
    program
        { init = initialModel ! []
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
