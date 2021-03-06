module Markdown.Renderer exposing (render)

import Markdown.AST exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (class)


render : List Block -> List (Html a)
render blocks =
    List.map renderBlock blocks


renderBlock : Block -> Html a
renderBlock block =
    case block of
        ContainerBlock container ->
            renderContainer container

        LeafBlock leaf ->
            renderLeaf leaf


renderContainer : Container -> Html a
renderContainer container =
    case container of
        BlockQuote blocks ->
            Html.blockquote [] (render blocks)

        IndentedCode code ->
            Html.pre [] [ Html.code [] [ Html.text code ] ]

        FencedCode info code ->
            let
                attrs : List (Html.Attribute a)
                attrs =
                    if info == "" then
                        []
                    else
                        [ class <| "language-" ++ info ]
            in
                Html.pre [] [ Html.code attrs [ Html.text code ] ]


renderLeaf : Leaf -> Html a
renderLeaf leaf =
    case leaf of
        Paragraph content ->
            Html.p [] (renderInline content)

        ThematicBreak ->
            Html.hr [] []

        Heading level content ->
            Html.node
                ("h" ++ toString level)
                []
                (renderInline content)


renderInline : Inline -> List (Html a)
renderInline content =
    [ Html.text content ]
