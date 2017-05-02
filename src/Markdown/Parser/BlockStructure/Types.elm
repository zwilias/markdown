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
    emptyContainer Document


emptyContainer : ContainerType -> Container
emptyContainer containerType =
    Container containerType []


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
    | BlockQuote


type Leaf
    = Text String
    | EmptyLine String
    | ThematicBreak
