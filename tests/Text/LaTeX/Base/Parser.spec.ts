import "mocha";

import {expect} from "chai";
import {
    mathTypes, newFixArg, newOptArg, newTeXComm, newTeXComment, newTeXEnv, newTeXMath, newTeXMathDol, newTeXRaw,
    TeXComment
} from "../../../../sources/Text/LaTeX/Base/Syntax";
import {
    fixArg,
    command, comment, dolMath, isSpecialCharacter, latexBlockParser, mustBeOk,
    text, environment
} from "../../../../sources/Text/LaTeX/Base/Parser";
import {custom, Result, Success} from "parsimmon";


describe("Parser", () => {

    it("comment", () => {
        expect(mustBeOk(comment.parse(`% u h h h `)).value).to.deep.equal(
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
        expect(mustBeOk(command.parse(`\\`)).value).to.deep.equal({});
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
        expect(mustBeOk(text.parse("lol")).value).to.deep.equal(
            newTeXRaw("lol")
        );
        expect(mustBeOk(text.parse(" l o l ")).value).to.deep.equal(
            newTeXRaw(" l o l ")
        );
        expect(mustBeOk(text.parse(" ")).value).to.deep.equal(
            newTeXRaw(" ")
        );
        expect(text.parse("").status).to.be.false;
        expect(text.parse("l%ol").status).to.be.false;
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
        });
    });

    describe("Math", () => {
        it("fixArg", () => {
            expect(mustBeOk(fixArg.parse("{}")).value).to.deep.equal(
                newFixArg([])
            );
            expect(mustBeOk(fixArg.parse(`{txt
            and %comment
            txt
            }`)).value).to.deep.equal(
                newFixArg([
                    {
                        "text": "txt\n            and ",
                        "type": "TeXRaw"
                    },
                    {
                        "text": "comment",
                        "type": "TeXComment"
                    },
                    {
                        "text": "            txt\n            ",
                        "type": "TeXRaw"
                    }
                ])
            );
        });

        it("dolMath", () => {
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
        it("math", () => {
            expect(mustBeOk(comment.parse(`% u h h h `)).value).to.deep.equal(
                newTeXComment(" u h h h ")
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