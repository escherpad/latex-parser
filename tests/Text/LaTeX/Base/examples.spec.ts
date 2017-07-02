import "mocha";

import {expect} from "chai";
import {
    mathTypes, newFixArg, newOptArg, newTeXComm, newTeXComment, newTeXMath, newTeXMathDol, newTeXRaw,
    TeXComment
} from "../../../../sources/Text/LaTeX/Base/Syntax";
import {
    fixArg,
    command, comment, dolMath, isSpecialCharacter, latexBlockParser, mustBeOk,
    text, latexParser
} from "../../../../sources/Text/LaTeX/Base/Parser";
import {custom, Result, Success} from "parsimmon";


describe("Parser", () => {
    describe("should parse", () => {
        it("example1", () => {
            expect(mustBeOk(latexParser.parse(
                `\\documentclass{article}

\\newenvironment{foo}{\\begin{center}}{\\end{center}}

\\begin{document}

\\end{document}
`
            )).value).to.deep.equal(
                [
                    {
                        "name": "documentclass",
                        "arguments": [
                            {
                                "type": "FixArg", "latex": [
                                {
                                    "text": "article",
                                    "type": "TeXRaw"
                                }
                            ]
                            }
                        ],
                        "type": "TeXComm"
                    },
                    {
                        "text": "\n\n",
                        "type": "TeXRaw"
                    },
                    {
                        "name": "newenvironment",
                        "arguments": [
                            {
                                "type": "FixArg", "latex": [{"text": "foo", "type": "TeXRaw"}]
                            }
                        ],
                        "type": "TeXComm"
                    },
                    [
                        ["}{"],
                        [{"text": "center", "type": "TeXRaw"}]
                    ], {"text": "\n\n", "type": "TeXRaw"}, {
                    "name": "begin",
                    "arguments": [{"type": "FixArg", "latex": [{"text": "document", "type": "TeXRaw"}]}],
                    "type": "TeXComm"
                }, {"text": "\n\n", "type": "TeXRaw"}, {
                    "name": "end",
                    "arguments": [{"type": "FixArg", "latex": [{"text": "document", "type": "TeXRaw"}]}],
                    "type": "TeXComm"
                }, {"text": "\n", "type": "TeXRaw"}
                ]
            );
        });
    });
});