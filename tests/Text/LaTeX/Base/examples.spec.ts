import "mocha";

import {expect} from "chai";
import {
    mathTypes, newFixArg, newOptArg, newTeXComm, newTeXComment, newTeXEnv, newTeXMath, newTeXMathDol, newTeXRaw,
    TeXComment, TeXRaw
} from "../../../../src/Text/LaTeX/Base/Syntax";
import {
    fixArg,
    command, comment, dolMath, isSpecialCharacter, latexBlockParser, mustBeOk,
    text, latexParser
} from "../../../../src/Text/LaTeX/Base/Parser";
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
                    newTeXComm("documentclass", newFixArg([newTeXRaw("article")])),
                    newTeXRaw("\n\n"),
                    newTeXComm("newenvironment",
                        newFixArg([newTeXRaw("foo")]),
                        newFixArg([newTeXComm("begin", newFixArg([newTeXRaw("center")]))]),
                        newFixArg([newTeXComm("end", newFixArg([newTeXRaw("center")]))])
                    ),
                    newTeXRaw("\n\n"),
                    newTeXEnv("document", [newTeXRaw("\n\n")]),
                    newTeXRaw("\n"),
                ]
            );
        });
    });
});