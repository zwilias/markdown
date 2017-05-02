module Markdown.Parser exposing (parse)

import Markdown.Parser.BlockStructure as BlockStructure


parse : String -> BlockStructure.ParseResult
parse =
    String.lines
        >> BlockStructure.processLines
