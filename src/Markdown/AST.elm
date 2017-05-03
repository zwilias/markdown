module Markdown.AST exposing (..)


type Block
    = ContainerBlock Container
    | LeafBlock Leaf


type Container
    = BlockQuote (List Block)


type alias Inline =
    String


type Leaf
    = Paragraph Inline
    | ThematicBreak
