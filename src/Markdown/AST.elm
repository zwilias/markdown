module Markdown.AST exposing (..)


type Block
    = ContainerBlock Container
    | LeafBlock Leaf


type Container
    = BlockQuote (List Block)
    | IndentedCode String


type alias Inline =
    String


type Leaf
    = Paragraph Inline
    | ThematicBreak
    | Heading Int Inline
