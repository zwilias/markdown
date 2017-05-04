module Test exposing (view)

import Html exposing (Html)
import Markdown.Parser exposing (parse)
import Markdown.Renderer exposing (render)
import Source exposing (source)


view : Html a
view =
    case parse source of
        Err _ ->
            Debug.crash "encountered parsing error"

        Ok doc ->
            Html.node "doc" [] (render doc)
