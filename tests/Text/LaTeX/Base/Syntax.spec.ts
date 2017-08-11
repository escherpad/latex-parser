import "mocha";
import {expect} from "chai";
import {
    LaTeXRaw,
    newFixArg,
    newOptArg,
    newSubOrSuperScript,
    newTeXComm, newTeXMath, newTeXRaw,
    stringifyLaTeX,
    SubOrSuperSymbol
} from "../../../../src/Text/LaTeX/Base/Syntax";
import {latexParser, mustBeOk} from "../../../../src/Text/LaTeX/Base/Parser";
import {Success} from "parsimmon";
import {mustNotBeUndefined} from "../../../../src/Utils";

describe("Syntax", () => {
    it("getBody", () => {
        // TODO
        // expect(JSON.stringify(getBody(
        //         newTeXEnv("document", [{}], newFixArg([{}]))
        //     ))).to.equal("");
    });
    it("stringify cmd", () => {
        expect(
            stringifyLaTeX(newTeXComm(
                "aCmd",
                newOptArg([newTeXRaw("optArg")]),
                newFixArg([newTeXRaw("fixArg")])
                )
            )
        ).to.eq(
            "\\aCmd[optArg]{fixArg}"
        );
    });

    it("stringify math", () => {
        expect(
            stringifyLaTeX(newTeXMath(
                "Dollar",
                "start",
                "end",
                [
                    newSubOrSuperScript(SubOrSuperSymbol.SUP, "_", [newOptArg([newTeXRaw("optArg")]), newFixArg([newTeXRaw("fixArg")])]),
                    newSubOrSuperScript(SubOrSuperSymbol.SUB, "^", [newOptArg([newTeXRaw("optArg")]), newFixArg([newTeXRaw("fixArg")])])
                ]
                )
            )
        ).to.eq(
            "start_[optArg]{fixArg}^[optArg]{fixArg}end"
        );
    });

    it("stringify src", () => {
        const src = `\\cmd{arg} txt % cmt
        and then $ ^{m}ath _{mod}-e$`;
        const success2: Success<LaTeXRaw[] | undefined> = mustBeOk(latexParser.parse(src));
        const success: LaTeXRaw[] | undefined = success2.value;
        const parsed: LaTeXRaw[] = mustNotBeUndefined(success);

        expect(
            parsed.map(l => stringifyLaTeX(l)).join("")
        ).to.eq(
            src
        );
    });
});