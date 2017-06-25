# JavaScript LaTeX parser
[![npm version](https://badge.fury.io/js/latex-parser.svg)](https://badge.fury.io/js/latex-parser)
[![Build Status](https://travis-ci.org/digitalheir/latex-parser.svg?branch=master)](https://travis-ci.org/digitalheir/latex-parser)
[![npm version](https://badge.fury.io/js/latex-parser.svg)](https://badge.fury.io/js/latex-parser)
![License](https://img.shields.io/npm/l/latex-parser.svg)
[![Code Climate](https://codeclimate.com/github/digitalheir/latex-parser/badges/gpa.svg)](https://codeclimate.com/github/digitalheir/latex-parser)

[Live demo in browser](https://digitalheir.github.io/latex-parser/)

This is a library designed to build abstract syntax trees for LaTeX documents using JavaScript / TypeScript.

This project is a TypeScript fork of the [**TeXnous project**](http://texnous.org). The original source code has been ported to TypeScript, and is compiled to ES5 with commonjs requires to use through NPM and as minified ES5 UMD module for direct use in the browser.

Implemented functionality requires LaTeX symbols, commands and environments contained in analyzed files to be described and provided to the parser object.

## Usage

```js
import {LatexStyle, LatexParser} from "latex-parser";

const latexStyle = new LatexStyle();
latexStyle.loadPackage("demo", {
  symbols: [{
      pattern: "\\\\"
  }],
  commands: [{
      name: "author",
      pattern: "[#1]#2",
      modes: {TEXT: true},
      parameters: [{}, {}],
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
  }, 
   {
      name: "document",
      modes: {TEXT: true}
  }, 
  {
      name: "enddocument",
      modes: {TEXT: true}
  }
  ],
  environments: [{
      name: "document",
      modes: {TEXT: true}
  }]
});


const latexParser = new LatexParser(latexStyle);

const tokens = latexParser.parse("hello \\author[opt]{name}");
```

See [live demo in browser](https://digitalheir.github.io/latex-parser/)
