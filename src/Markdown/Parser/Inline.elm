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
                    gatherText blocks
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

        _ ->
            Debug.crash "BS.Document not allowed at this level"


gatherText : List BS.Block -> ( AST.Leaf, List BS.Block )
gatherText blocks =
    gatherTextHelper ( [], blocks )


gatherTextHelper : ( List String, List BS.Block ) -> ( AST.Leaf, List BS.Block )
gatherTextHelper ( strings, blocks ) =
    case blocks of
        (BS.LeafBlock (BS.Text t)) :: rest ->
            gatherTextHelper ( t :: strings, rest )

        (BS.LeafBlock (BS.SetextHeading level _)) :: rest ->
            ( AST.Heading level <| String.join "\n" <| List.reverse strings, rest )

        _ ->
            ( AST.Paragraph <| String.join "\n" <| List.reverse strings, blocks )
