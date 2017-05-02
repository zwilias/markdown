module Markdown.Parser.BlockStructure.Types exposing (..)


type alias Stack =
    { current : Container
    , parents : List Container
    }


emptyStack : Stack
emptyStack =
    Stack emptyDocument []


emptyDocument : Container
emptyDocument =
    Container Document []


type Block
    = ContainerBlock Container
    | LeafBlock Leaf


addBlockToContainer : Block -> Container -> Container
addBlockToContainer block container =
    { container | children = block :: container.children }


type alias Container =
    { type_ : ContainerType
    , children : List Block
    }


type ContainerType
    = Document


type Leaf
    = Text String
    | EmptyLine String
    | ThematicBreak
