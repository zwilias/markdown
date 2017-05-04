module Markdown.Parser.BlockStructure.Types exposing (..)

import Dict exposing (Dict)


type alias Stack =
    { current : Container
    , parents : List Container
    }


type alias StackState =
    { stack : Stack
    , lastLineIsText : Bool
    , unmatched : Int
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


type alias RefMap =
    Dict String String


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
    | SetextHeading Int String
    | ThematicBreak
    | ATXHeading Int String
