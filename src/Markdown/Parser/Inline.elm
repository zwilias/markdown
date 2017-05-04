module Markdown.Parser.Inline exposing (..)

import Markdown.Parser.BlockStructure.Types as BS
import Markdown.AST as AST
import Dict exposing (Dict)


parseInlineContent :
    ( List BS.Block, Dict String String )
    -> List AST.Block
parseInlineContent ( rawBlocks, refMap ) =
    blocksToAST refMap rawBlocks


blocksToAST : BS.RefMap -> List BS.Block -> List AST.Block
blocksToAST refMap blocks =
    case blocks of
        [] ->
            []

        (BS.LeafBlock (BS.Text t)) :: rest ->
            let
                ( leaf, otherBlocks ) =
                    paragraphOrSetext blocks
            in
                (AST.LeafBlock leaf)
                    :: blocksToAST refMap otherBlocks

        (BS.LeafBlock (BS.EmptyLine _)) :: rest ->
            blocksToAST refMap rest

        (BS.LeafBlock BS.ThematicBreak) :: rest ->
            AST.LeafBlock AST.ThematicBreak
                :: blocksToAST refMap rest

        (BS.LeafBlock (BS.SetextHeading _ content)) :: rest ->
            Debug.log "Encountered a stray setext heading. Making it a paragraph, for now" (AST.LeafBlock (AST.Paragraph content))
                :: blocksToAST refMap rest

        (BS.LeafBlock (BS.ATXHeading lvl content)) :: rest ->
            AST.LeafBlock (AST.Heading lvl content)
                :: blocksToAST refMap rest

        (BS.ContainerBlock container) :: rest ->
            containerToAST refMap container :: blocksToAST refMap rest


containerToAST : BS.RefMap -> BS.Container -> AST.Block
containerToAST refMap { type_, children } =
    case type_ of
        BS.BlockQuote ->
            AST.ContainerBlock <| AST.BlockQuote (blocksToAST refMap children)

        BS.IndentedCode ->
            AST.ContainerBlock <| AST.IndentedCode (gatherText children)

        _ ->
            Debug.crash "BS.Document not allowed at this level"


gatherText : List BS.Block -> String
gatherText blocks =
    case blocks of
        [] ->
            ""

        (BS.LeafBlock (BS.Text t)) :: rest ->
            t ++ "\n" ++ (gatherText rest)

        (BS.LeafBlock (BS.EmptyLine t)) :: rest ->
            t ++ "\n" ++ (gatherText rest)

        _ ->
            Debug.crash "This looks a container _in_ a code-block."


paragraphOrSetext : List BS.Block -> ( AST.Leaf, List BS.Block )
paragraphOrSetext blocks =
    paragraphOrSetextHelper ( [], blocks )


paragraphOrSetextHelper : ( List String, List BS.Block ) -> ( AST.Leaf, List BS.Block )
paragraphOrSetextHelper ( strings, blocks ) =
    case blocks of
        (BS.LeafBlock (BS.Text t)) :: rest ->
            paragraphOrSetextHelper ( t :: strings, rest )

        (BS.LeafBlock (BS.SetextHeading level _)) :: rest ->
            ( AST.Heading level <| String.join "\n" <| List.reverse strings, rest )

        _ ->
            ( AST.Paragraph <| String.join "\n" <| List.reverse strings, blocks )
