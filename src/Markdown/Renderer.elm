module Markdown.Renderer exposing (render)

import Markdown.AST exposing (..)
import Html exposing (Html)


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


renderLeaf : Leaf -> Html a
renderLeaf leaf =
    case leaf of
        Paragraph content ->
            Html.p [] (renderInline content)

        ThematicBreak ->
            Html.hr [] []


renderInline : Inline -> List (Html a)
renderInline content =
    [ Html.text content ]
