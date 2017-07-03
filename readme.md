# JavaScript LaTeX parser
[![npm version](https://badge.fury.io/js/latex-parser.svg)](https://badge.fury.io/js/latex-parser)
[![Build Status](https://travis-ci.org/digitalheir/latex-parser.svg?branch=master)](https://travis-ci.org/digitalheir/latex-parser)
[![npm version](https://badge.fury.io/js/latex-parser.svg)](https://badge.fury.io/js/latex-parser)
![License](https://img.shields.io/npm/l/latex-parser.svg)
[![Code Climate](https://codeclimate.com/github/digitalheir/latex-parser/badges/gpa.svg)](https://codeclimate.com/github/digitalheir/latex-parser)

[Live demo in browser](https://digitalheir.github.io/latex-parser/)


This is a library designed to build abstract syntax trees for LaTeX documents using JavaScript / TypeScript.

## ⚠ Warning
This project only parses a subset of what can be considered "canonical" LaTeX. As I see it, a full LaTeX parser would be a complete port of the TeX code, and [I am not crazy enough to attempt that](https://stackoverflow.com/a/3814911/673206). Of course, we can still make something that works *most* of the time. This approach is similar to [KaTeX](https://github.com/Khan/KaTeX), a Javascript library which focuses on math typesetting.

If you don't believe parsing LaTeX is necessarily difficult, prepare to be outsmugged:

* [Is there a BNF grammar of the TeX language?](https://tex.stackexchange.com/questions/4201/is-there-a-bnf-grammar-of-the-tex-language)
* [Full LaTeX parse in Java](https://stackoverflow.com/questions/13777558/full-latex-parser-in-java)
* [Why is LaTeX so complicated?](
https://tex.stackexchange.com/questions/222500/why-is-latex-so-complicated)


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

## Contribute 
Pull requests are welcome. If you find a bug or an omission, don't be afraid to [open an issue](https://github.com/digitalheir/latex-parser/issues).

## History on project
This project used to be a TypeScript fork of the [TeXnous project](http://texnous.org), but after v0.3.0 is modeled after the Haskell LaTeX library [HaTeX](https://github.com/Daniel-Diaz/HaTeX).

## Contact
Inquiries go to maartentrompper@freedom.nl.
