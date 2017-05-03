module Markdown.Parser exposing (parse)

import Markdown.Parser.BlockStructure as BlockStructure
import Markdown.Parser.Inline as Inline
import Markdown.AST as AST
import Parser


parse : String -> Result Parser.Error (List AST.Block)
parse =
    String.lines
        >> BlockStructure.processLines
        >> Result.map Inline.parseInlineContent
