import "mocha";
import {expect} from "chai";
import {getBody, newFixArg, newTeXEnv} from "../../../../sources/Text/LaTeX/Base/Syntax";

describe("Syntax", () => {
    it("getBody", () => {
        expect(JSON.stringify(getBody(
                newTeXEnv("document", {}, newFixArg({}))
            ))).to.equal("");
    });
});