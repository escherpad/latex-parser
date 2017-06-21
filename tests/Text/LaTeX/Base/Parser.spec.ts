import "mocha";
import {expect} from "chai";
import {newTeXComment} from "../../../../sources/Text/LaTeX/Base/Syntax";
import {comment} from "../../../../sources/Text/LaTeX/Base/Parser";

describe("Parser", () => {
    it("comment", () => {
        expect(comment.parse(`%`)).to.deep.equal(newTeXComment(" u h h h "));
    });
});