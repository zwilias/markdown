#!/bin/bash

source=$(cat)

cat > ./Source.elm <<EOF
module Source exposing (source)

source : String
source = """
${source}
"""
EOF

../node_modules/.bin/elm-static-html -f Test.elm -o tmp.txt >/dev/null &&
    sed '1d;$d' tmp.txt
