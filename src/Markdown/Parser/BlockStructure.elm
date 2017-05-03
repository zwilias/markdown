module Markdown.Parser.BlockStructure exposing (processLines, ParseResult)

import Markdown.Parser.BlockStructure.Types exposing (..)
import Dict exposing (Dict)
import Parser
    exposing
        ( Parser
        , Count(..)
        , succeed
        , keep
        , zeroOrMore
        , map
        , map2
        , andThen
        , lazy
        , oneOf
        , fail
        , inContext
        , delayedCommitMap
        , repeat
        , oneOrMore
        , ignore
        , source
        , symbol
        , (|=)
        , (|.)
        )


type alias ParseResult =
    Result Parser.Error ( List Block, Dict String String )


processLines : List String -> ParseResult
processLines =
    List.foldl
        (\string stackR -> Result.andThen (processLine string) stackR)
        (Ok emptyStack)
        >> Result.map (closeStack >> extractReferences)


processLine : String -> Stack -> Result Parser.Error Stack
processLine line stack =
    Parser.run (lineParser stack) line


lineParser : Stack -> Parser Stack
lineParser { current, parents } =
    let
        currentContainers : List Container
        currentContainers =
            List.reverse <| current :: parents
    in
        tryOpenContainers currentContainers
            |> andThen (flip closeContainers (List.reverse currentContainers))
            |> andThen containersToStack
            |> flip2 map2 addContainersToStack (newContainers [])
            |> flip2 map2 addLeafToStack parseLeaf


flip2 : (a -> b -> c -> d) -> a -> c -> b -> d
flip2 f a1 a2 a3 =
    f a1 a3 a2


addContainersToStack : Stack -> List Container -> Stack
addContainersToStack stack containers =
    case containers of
        [] ->
            stack

        container :: rest ->
            addContainersToStack (addToStack stack container) rest


newContainers : List Container -> Parser (List Container)
newContainers containers =
    inContext "finding new containers" <|
        oneOf
            [ newContainer
                |> andThen (\c -> newContainers (c :: containers))
            , succeed <| List.reverse containers
            ]


addToStack : Stack -> Container -> Stack
addToStack stack container =
    { stack
        | current = container
        , parents = stack.current :: stack.parents
    }


newContainer : Parser Container
newContainer =
    oneOf [ blockQuote ]
        |> map emptyContainer
        |> inContext "checking for a new container"


blockQuote : Parser ContainerType
blockQuote =
    inContext "block quote" <|
        allOrNothing <|
            succeed BlockQuote
                |. nonIndent
                |. symbol ">"
                |. requiredSpaces


nonIndent : Parser ()
nonIndent =
    List.range 0 3
        |> List.reverse
        |> List.map
            (\x ->
                ignore (Exactly x) ((==) ' ')
            )
        |> oneOf


requiredSpaces : Parser ()
requiredSpaces =
    ignore oneOrMore ((==) ' ')


optionalSpaces : Parser ()
optionalSpaces =
    ignore zeroOrMore ((==) ' ')


parseLeaf : Parser Leaf
parseLeaf =
    inContext "Looking for a Leaf" <|
        oneOf
            [ thematicBreak
            , emptyLine
            , textLine
            ]


thematicBreak : Parser Leaf
thematicBreak =
    succeed ThematicBreak
        |. nonIndent
        |. oneOf
            [ repeatingChar '*'
            , repeatingChar '_'
            , repeatingChar '-'
            ]
        |. optionalSpaces
        |> allOrNothing
        |> inContext "thematic break"


repeatingChar : Char -> Parser ()
repeatingChar marker =
    succeed ()
        |. ignore (Exactly 1) ((==) marker)
        |. optionalSpaces
        |> repeat (AtLeast 3)
        |> map (always ())
        |> allOrNothing


textLine : Parser Leaf
textLine =
    inContext "Text line" <|
        succeed Text
            |= restOfLine


emptyLine : Parser Leaf
emptyLine =
    inContext "Empty line" <|
        map EmptyLine <|
            allOrNothing <|
                andThen
                    (\content ->
                        if String.length (String.trim content) > 0 then
                            fail "not an empty line"
                        else
                            succeed content
                    )
                    restOfLine


allOrNothing : Parser a -> Parser a
allOrNothing parser =
    delayedCommitMap always parser (succeed ())


addLeafToStack : Stack -> Leaf -> Stack
addLeafToStack { current, parents } leaf =
    let
        updatedCurrent : Container
        updatedCurrent =
            { current | children = (LeafBlock leaf) :: current.children }
    in
        Stack updatedCurrent parents


containersToStack : List Container -> Parser Stack
containersToStack containers =
    case containers of
        [] ->
            fail "Expected at least the Document to remain on the stack"

        top :: parents ->
            succeed <| Stack top parents


closeContainers : Int -> List Container -> Parser (List Container)
closeContainers count containerList =
    if count == 0 then
        succeed containerList
    else
        case containerList of
            container :: parent :: rest ->
                closeContainer container parent
                    :: rest
                    |> closeContainers (count - 1)

            _ ->
                fail <|
                    "Need to close "
                        ++ toString count
                        ++ "containers, but ran out."


closeContainer : Container -> Container -> Container
closeContainer container parent =
    let
        updatedContainer : Container
        updatedContainer =
            { container | children = List.reverse container.children }
    in
        addBlockToContainer (ContainerBlock updatedContainer) parent


tryOpenContainers : List Container -> Parser Int
tryOpenContainers containers =
    case containers of
        [] ->
            succeed 0

        { type_ } :: rest ->
            oneOf
                [ continueContainer type_
                    |> andThen (\_ -> (tryOpenContainers rest))
                , succeed (List.length containers)
                ]


continueContainer : ContainerType -> Parser ()
continueContainer containerType =
    case containerType of
        Document ->
            succeed ()

        BlockQuote ->
            allOrNothing blockQuote
                |> map (always ())


closeStack : Stack -> List Block
closeStack { current, parents } =
    case parents of
        [] ->
            List.reverse current.children

        parent :: rest ->
            Stack (closeContainer current parent) rest
                |> closeStack


restOfLine : Parser String
restOfLine =
    keep zeroOrMore (always True)


extractReferences : List Block -> ( List Block, Dict String String )
extractReferences blockList =
    ( blockList, Dict.empty )
