import "mocha";

import {expect} from "chai";

import {
    newFixArg,
    newOptArg,
    newSubOrSuperScript,
    newTeXComm,
    newTeXComment,
    newTeXEnv,
    newTeXMathDol,
    newTeXRaw, SubOrSuperSymbol,
    TeXComment
} from "../../../../src/Text/LaTeX/Base/Syntax";

import {
    fixArg,
    command, comment, dolMath, isSpecialCharacter, latexBlockParser, mustBeOk,
    environment, notTextDefault, textParser
} from "../../../../src/Text/LaTeX/Base/Parser";

describe("Parser", () => {

    it("comment", () => {

        const parse = comment.parse(`% u h h h `);
        const success = mustBeOk(parse);

        expect(success.value).to.deep.equal(
            newTeXComment(" u h h h ")
        );
        expect(mustBeOk(comment.parse("%a\n")).value).to.deep.equal(
            newTeXComment("a")
        );
        expect(comment.parse("%a\n ").status).to.be.false;
    });

    describe("environment", () => {
        it("must parse named env", () => {
            expect(mustBeOk(environment.parse(`\\begin{document}

\\end{document}`)).value).to.deep.equal(
                newTeXEnv("document", [newTeXRaw("\n\n")])
            );
        });
        it("must parse anonymous", () => {

        });
    });

    it("cmd", () => {
        expect(mustBeOk(command.parse(`\\`)).value).to.deep.equal(newTeXComm(""));
        expect(mustBeOk(command.parse(`\\^`)).value).to.deep.equal(newTeXComm("^"));
        expect(mustBeOk(command.parse(`\\aCmd`)).value).to.deep.equal(newTeXComm("aCmd"));
        expect(mustBeOk(command.parse(`\\aCmd[opt1][opt2]{fix}`)).value).to.deep.equal(
            newTeXComm("aCmd",
                newOptArg([newTeXRaw("opt1")]),
                newOptArg([newTeXRaw("opt2")]),
                newFixArg([newTeXRaw("fix")])
            )
        );
        expect(mustBeOk(command.parse(`\\aCmd{fix1}{fix2}`)).value).to.deep.equal(
            newTeXComm("aCmd",
                newFixArg([newTeXRaw("fix1")]),
                newFixArg([newTeXRaw("fix2")])
            )
        );
        expect(mustBeOk(command.parse(`\\aCmd{\\fix{fix11}}`)).value).to.deep.equal(
            newTeXComm("aCmd",
                newFixArg([newTeXComm("fix", newFixArg([newTeXRaw("fix11")]))])
            )
        );
        expect(mustBeOk(command.parse(`\\aCmd{\\fixOne{1}}{\\fixTwo{2}}`)).value).to.deep.equal(
            newTeXComm("aCmd",
                newFixArg([newTeXComm("fixOne", newFixArg([newTeXRaw("1")]))]),
                newFixArg([newTeXComm("fixTwo", newFixArg([newTeXRaw("2")]))])
            )
        );

        // expect(mustBeOk(comment.parse("%a\n")).value).to.deep.equal(
        //     newTeXComment("a")
        // );
        // expect(comment.parse("%a\n ").status).to.be.false;
    });


    // todo experiment nottext
    it("text", () => {
        expect(mustBeOk(textParser(notTextDefault).parse("lol")).value).to.deep.equal(
            newTeXRaw("lol")
        );
        expect(mustBeOk(textParser(notTextDefault).parse(" l o l ")).value).to.deep.equal(
            newTeXRaw(" l o l ")
        );
        expect(mustBeOk(textParser(notTextDefault).parse(" ")).value).to.deep.equal(
            newTeXRaw(" ")
        );
        expect(textParser(notTextDefault).parse("").status).to.be.false;
        expect(textParser(notTextDefault).parse("l%ol").status).to.be.false;
    });

    describe("latexBlockParser", () => {
        it("text", () => {
            expect(mustBeOk(latexBlockParser.parse("lol")).value).to.deep.equal(
                newTeXRaw("lol")
            );
            expect(mustBeOk(latexBlockParser.parse("l o l")).value).to.deep.equal(
                newTeXRaw("l o l")
            );
            expect(mustBeOk(latexBlockParser.parse(" lol ")).value).to.deep.equal(
                newTeXRaw(" lol ")
            );
            expect(mustBeOk(latexBlockParser.parse("\nlol\n")).value).to.deep.equal(
                newTeXRaw("\nlol\n")
            );
            expect(mustBeOk(environment.parse(`\\begin{document}

\\end{document}`)).value).to.deep.equal(
                newTeXEnv("document", [newTeXRaw("\n\n")])
            );
        });
    });


    describe("Args", () => {
        it("fixArg", () => {
            expect(mustBeOk(fixArg.parse("{}")).value).to.deep.equal(
                newFixArg([])
            );
            expect(mustBeOk(fixArg.parse(`{txt
            and %comment
            txt
            }`)).value).to.deep.equal(
                newFixArg([
                    newTeXRaw("txt\n            and "),
                    {
                        "text": "comment",
                        "type": "TeXComment"
                    },
                    newTeXRaw("            txt\n            ")
                ])
            );
        });
    });

    describe("Math", () => {

        it("should recognize math mode", () => {
            expect(mustBeOk(dolMath.parse(`$$`)).value).to.deep.equal(
                newTeXMathDol([])
            );

            expect(mustBeOk(dolMath.parse(`$ $`)).value).to.deep.equal(
                newTeXMathDol([newTeXRaw(" ")])
            );

            expect(mustBeOk(dolMath.parse(`$ lol $`)).value).to.deep.equal(
                newTeXMathDol([newTeXRaw(" lol ")])
            );

        });

        it("should parse superscript and subscript", () => {
            expect(mustBeOk(dolMath.parse(`$ 1_{a} 2^{b} $`)).value).to.deep.equal(
                newTeXMathDol([
                    newTeXRaw(" 1"),
                    newSubOrSuperScript(SubOrSuperSymbol.SUB, "_", [newFixArg([newTeXRaw("a")])]),
                    newTeXRaw(" 2"),
                    newSubOrSuperScript(SubOrSuperSymbol.SUP, "^", [newFixArg([newTeXRaw("b")])]),
                    newTeXRaw(" "),
                ])
            );
        });
    });


    it("isSpecial", () => {
        const customSpecialChars: { [k: string]: boolean } = {
            "b": true
        };
        expect(isSpecialCharacter("a")).to.be.false;
        expect(isSpecialCharacter("a", customSpecialChars)).to.be.false;
        expect(isSpecialCharacter("b", customSpecialChars)).to.be.true;

        expect(isSpecialCharacter("'")).to.be.true;
        expect(isSpecialCharacter("(")).to.be.true;
        expect(isSpecialCharacter(")")).to.be.true;
        expect(isSpecialCharacter(",")).to.be.true;
        expect(isSpecialCharacter(".")).to.be.true;
        expect(isSpecialCharacter("-")).to.be.true;
        expect(isSpecialCharacter('"')).to.be.true;
        expect(isSpecialCharacter("!")).to.be.true;
        expect(isSpecialCharacter("^")).to.be.true;
        expect(isSpecialCharacter("$")).to.be.true;
        expect(isSpecialCharacter("&")).to.be.true;
        expect(isSpecialCharacter("#")).to.be.true;
        expect(isSpecialCharacter("{")).to.be.true;
        expect(isSpecialCharacter("}")).to.be.true;
        expect(isSpecialCharacter("%")).to.be.true;
        expect(isSpecialCharacter("~")).to.be.true;
        expect(isSpecialCharacter("|")).to.be.true;
        expect(isSpecialCharacter("/")).to.be.true;
        expect(isSpecialCharacter(":")).to.be.true;
        expect(isSpecialCharacter(";")).to.be.true;
        expect(isSpecialCharacter("=")).to.be.true;
        expect(isSpecialCharacter("[")).to.be.true;
        expect(isSpecialCharacter("]")).to.be.true;
        expect(isSpecialCharacter("\\")).to.be.true;
        expect(isSpecialCharacter("`")).to.be.true;
        expect(isSpecialCharacter(" ")).to.be.true;
    });
});