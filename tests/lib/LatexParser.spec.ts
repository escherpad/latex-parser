/**
 * @fileoverview  LaTeX parser class tests
 * This file is a part of TeXnous project.
 *
 * @copyright TeXnous project team (http://texnous.org) 2016
 * @license LGPL-3.0
 *
 * This unit test is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version 3
 * of the License, or (at your option) any later version.
 *
 * This unit test is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this unit
 * test; if not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA.
 */

import {expect} from "chai";

import {LatexStyle, LatexParser} from "../../sources/main";

const latexStyle = new LatexStyle();

latexStyle.loadPackage("test", {
    symbols: [
        {
            pattern: "\\\\",
        },
        {
            pattern: "\""
        }
    ],
    commands: [{
        name: "author",
        pattern: "[#1]#2",
        modes: {TEXT: true},
        parameters: [{}, {}],
        operations: []
    }, {
        name: "\\",
        pattern: "",
        modes: {TEXT: true},
        parameters: [],
        operations: []
    }, {
        name: "author",
        pattern: " [#1]#2",
        modes: {TEXT: true},
        parameters: [{}, {}],
        operations: []
    }, {
        name: "author",
        pattern: "#1",
        modes: {TEXT: true},
        parameters: [{}],
        operations: []
    }, {
        name: '"',
        pattern: "[#1]#2",
        modes: {TEXT: true},
        parameters: [{}, {}],
        operations: []
    }, {
        name: '"',
        pattern: " [#1]#2",
        modes: {TEXT: true},
        parameters: [{}, {}],
        operations: []
    }, {
        name: '"',
        pattern: "#1",
        modes: {TEXT: true},
        parameters: [{}],
        operations: []
    }, {
        name: "document",
        modes: {TEXT: true}
    }, {
        name: "enddocument",
        modes: {TEXT: true}
    }],
    environments: [{
        name: "document",
        modes: {TEXT: true}
    }]
});

let latexParser: LatexParser;


describe("LatexParser", () => {
    /**
     * LaTeX parser object constructor test
     */
    latexParser = new LatexParser(latexStyle);

    /**
     * Spaces and analogs handling tests
     */
    it("parse spaces", function () {
        expect(latexParser.parse("").join("")).to.equal("");
        expect(latexParser.parse("% comment\n % comment").join("")).to.equal("");
        expect(latexParser.parse(" ").join("\n")).to.equal("SpaceToken{ }");
        expect(latexParser.parse(" % comment\n ").join("")).to.equal("SpaceToken{ }");
        expect(latexParser.parse("\t% comment\n ").join("")).to.equal("SpaceToken{ }");
        expect(latexParser.parse("\t% comment\n\n").join("")).to.equal("SpaceToken{\n}");
        expect(latexParser.parse("\n % comment\n\n").join("")).to.equal("SpaceToken{\n\n}");
        expect(latexParser.parse("\n % comment\n\n  % comment\n\n").join("")).to.equal("SpaceToken{\n\n}");

    });
    /**
     * LaTeX symbols handling tests
     */
    it("parse symbols", function () {

        expect(latexParser.parse(
            "{{}} "
        ).join("")).to.equal(
            "LatexTree.SymbolToken { \"{{}}\" } 0-3" +
            "LatexTree.SpaceToken { \" \" } 4-4"
        );
        expect(latexParser.parse(
            "{{}}%\n\n "
        )).to.equal(
            "LatexTree.SymbolToken { \"{{}}\" } 0-3" +
            "LatexTree.SpaceToken { \"\n\" } 6-7"
        );

        // TODO what should the parser do here? CommandToken[?] or symbol?
        expect(latexParser.parse("%\n\\\\%\n").join("")).to.equal("CommandToken{\\\\}");
        expect(latexParser.parse(" \\\\").join("")).to.equal("SpaceToken{ }CommandToken{\\\\}");
        expect(latexParser.parse("\\\\ ").join("")).to.equal("CommandToken{\\\\}SpaceToken{ }");
        expect(latexParser.parse("\\\\\\\\").join("")).to.equal("CommandToken{\\\\}CommandToken{\\\\}");
        expect(latexParser.parse('"%\n').join("")).to.equal('SymbolToken{"}');
        expect(latexParser.parse('\\\\"').join("")).to.equal('CommandToken{\\\\}SymbolToken{"}');
        expect(latexParser.parse('%\n"\\\\').join("")).to.equal('SymbolToken{"}CommandToken{\\\\}');
        expect(latexParser.parse('"%\n"').join("")).to.equal('SymbolToken{"}SymbolToken{"}');
        expect(latexParser.parse('\'%\n"').join("")).to.equal("SymbolToken[?]{'}SymbolToken{\"}");
    });

    /**
     * LaTeX commands handling tests
     */
    it("parse commands", function () {
        expect(latexParser.parse('\\"o').join("")).to.equal('CommandToken{\\"o}');
        expect(latexParser.parse('\\"[b]o').join("")).to.equal('CommandToken{\\"[b]o}');
        expect(latexParser.parse('\\"[b]{oo}').join("")).to.equal('CommandToken{\\"[b]{oo}}');

        expect(latexParser.parse("\\author{Name}").join("")).to.equal("CommandToken{\\author{Name}}");
        expect(latexParser.parse("\\author [Opt Name] {Name}").join("")).to.equal("CommandToken{\\author [Opt Name] {Name}}");
        expect(latexParser.parse("\\author[{Opt Name}] {Name}").join("")).to.equal("CommandToken{\\author[{Opt Name}] {Name}}");
        expect(latexParser.parse('\\"[{Opt Name}] {Name}').join("")).to.equal('CommandToken{\\"[{Opt Name}] {Name}}');
    });
    /**
     * LaTeX environments handling tests
     */
    it("parse environments", function () {
        expect(latexParser.parse("\\begin{document}\\end{document}").join()).to.equal("EnvironmentToken{\\begin{document}\\end{document}}");
        expect(latexParser.parse("\\begin {document}\\author{Name}\\end{document}").join()).to.equal("EnvironmentToken{\\begin{document}\\author{Name}\\end{document}}");
        //
    });
});
