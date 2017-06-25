import "mocha";

import {expect} from "chai";
import {
    mathTypes, newTeXComment, newTeXMath, newTeXMathDol, newTeXRaw,
    TeXComment
} from "../../../../sources/Text/LaTeX/Base/Syntax";
import {comment, dolMath, isSpecialCharacter, latexBlockParser, text} from "../../../../sources/Text/LaTeX/Base/Parser";
import {custom, Result, Success} from "parsimmon";


function isOk<T>(parse: Result<T>): parse is Success<T> {
    return parse.status === true;
}

function mustBeOk<T>(parse: Result<T>): Success<T> {
    if (!isOk(parse)) throw new Error("Expected parse to be success: " + JSON.stringify(parse));
    return parse;
}

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


    // todo experiment nottext
    it("text", () => {
        expect(mustBeOk(text.parse("lol")).value).to.deep.equal(
            newTeXRaw("lol")
        );
        expect(mustBeOk(text.parse(" l o l ")).value).to.deep.equal(
            newTeXRaw(" l o l ")
        );
        it("text", () => {
            expect(text.parse("l%ol").status).to.be.false;
        });
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
        it("dolMath", () => {
            // todo
            // expect(mustBeOk(dolMath.parse(`$$`)).value).to.deep.equal(
            //     newTeXMathDol({})
            // );

            expect(mustBeOk(dolMath.parse(`$ $`)).value).to.deep.equal(
                newTeXMathDol(newTeXRaw(" "))
            );

            expect(mustBeOk(dolMath.parse(`$ lol $`)).value).to.deep.equal(
                newTeXMathDol(newTeXRaw(" lol "))
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