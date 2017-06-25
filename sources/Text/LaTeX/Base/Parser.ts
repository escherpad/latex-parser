// <reference types="../../../../../../types/parsimmon.d.ts" />

import {seq, alt, regexp, string, Parser, lazy, takeWhile} from "parsimmon";

import {LaTeX, MathType, MathTypeHaving, newTeXComment, newTeXMath, TeXComment, TeXMath} from "./Syntax";
import {TeXRaw} from "./Syntax";
import {newTeXRaw, newTeXMathDol} from "./Syntax";

/** The /LaTeX/ parser.

 Use 'parseLaTeX' to parse a 'Text' containing /LaTeX/ code.
 If the 'Text' is in a file, you may want to use 'parseLaTeXFile'.
 Use this module together with "Text.LaTeX.Base.Syntax" to perform
 analysis and transformations of /LaTeX/ code. The parser ('parseLaTeX')
 is related with the renderer ('render') by the following property:

 /If @t :: Text@ is a syntactically valid LaTeX block, then:/

 > fmap render (parseLaTeX t) == Right t

 This property says two things:

 * Given a valid LaTeX input, 'parseLaTeX' returns a 'LaTeX' value.
 * If the parsed value is again rendered, you get the initial input.

 In other words, 'parseLaTeX' is a partial function defined over the
 set of valid LaTeX files, and 'render' is its /left/ inverse.
 */

//     -- * The parser
//     parseLaTeX
//   , parseLaTeXFile
//     -- * Parsing errors
//   , ParseError
//   , errorPos
//   , errorMessages
//     -- ** Error messages
//   , Message (..)
//   , messageString
//     -- ** Source positions
//   , SourcePos
//   , sourceLineKeep in mind that automatic inclusion is only important if you’re using files with global declarations (as opposed to files declared as modules). If you use an import "foo" statement, for instance, TypeScript may still look through node_modules & node_modules/types folders to find the foo package.


//   , sourceColumn
//   , sourceName
//     -- * Configuring your parser
//   , ParserConf (..)
//   , defaultParserConf
//   , parseLaTeXWith
//   , parseLaTeXFileWith
//     -- * Parser combinators
//   , Parser
//   , latexParser
//   , latexBlockParser
//     ) where
//
// import           Text.Parsec hiding ((<|>),many)
// import           Text.Parsec.Error
// import           Data.Char (toLower,digitToInt)
// import           Data.Monoid
// import           Data.Maybe (fromMaybe)
// import qualified Data.Text as T
//
// import           Control.Applicative
// import           Control.Monad (unless)
//
// import           Text.LaTeX.Base.Syntax
// import           Text.LaTeX.Base.Render

//
// Parser configuration
//

/** Configuration for the LaTeX parser.*/
export interface ParserConf {
    /**  This is the list of names of the environments such that
     their content will be parsed verbatim.*/
    verbatimEnvironments: String[];
}

/** Default parser configuration, used by 'parseLaTeX' and 'parseLaTeXFile'.

 Defaults:

 > verbatimEnvironments = ["verbatim"]
 */
export const defaultParserConf: ParserConf = {
    verbatimEnvironments: ["verbatim"]
};

/** Parser with 'Text' input and 'ParserConf' environment.
 */
// type Parser<T> = Parsec Text ParserConf;

//
// Parser
//

// /** Parse a 'Text' sequence as a 'LaTeX' block. If it fails, it returns
//  an error string.*/
// export const parseLaTeX = (s: string): (ParseError | LaTeX) => {
//     return parseLaTeXWith(defaultParserConf, s);
// };
//

//
// export const parseLaTeX = (conf: ParserConf, t: string): (ParseError | LaTeX) => {
//     return t === "" ? {} : runParser(latexParser,conf,"parseLaTeX input",t);
// };

// /** Read a file and parse it as 'LaTeX'.*/
// // TODO
//
// // export const parseLaTeXFile = (FilePath): IO (ParseError | LaTeX) => {
// // }
// // parseLaTeXFile = parseLaTeXFileWith defaultParserConf
// // parseLaTeXFileWith :: ParserConf -> FilePath -> IO (Either ParseError LaTeX)
// // parseLaTeXFileWith conf fp = runParser latexParser conf fp <$> readFileTex fp
//
// /** The 'LaTeX' parser.*/
// // latexParser :: Parser LaTeX
// // latexParser = mconcat <$> latexBlockParser `manyTill` eof
//

export const takeTill = (predicate: ((c: string) => boolean)) => takeWhile((c) => !predicate(c));

const takeTillNewline = regexp(/[^\n]*/);
const maybeNewline = regexp(/\n?/);
const whitespace = regexp(/\s*/m);
const commentSymbol = string("%");

function token(parser: Parser<string>): Parser<string> {
    return parser.skip(whitespace);
}

// Several parsers are just strings with optional whitespace.
function word(str: string): Parser<string> {
    return string(str).thru(token);
}


const lbrace = word("{");
const rbrace = word("}");
const lbracket = word("[");
const rbracket = word("]");
const comma = word(",");
const colon = word(":");


/** Text is a sequence on characters that are not non-text*/
// TODO use character codes
export const text = takeTill(c => isNotText(c, notTextDefault))
    .map(match => newTeXRaw(match));

/**Text without stopping on ']'*/
    // TODO

    // text2:
    // _ <- char ']'
    // t <- try (text <|> return (TeXRaw T.empty))
    // return $ TeXRaw (T.pack "]") <> t

const notRightBraceSequence = regexp(/[^}]*/);
const spaces: Parser<TeXRaw> = regexp(/ */)
    .map(newTeXRaw);

const anonym = lbrace
    .then(notRightBraceSequence)
    .skip(rbrace);

const env = word("\\begin")
    // .then()  // envName
    // .then() // TODO { bla bla bla bla }
    // env body
        .then(spaces)
        .then(word("\\end"))
    // .then(cmdArgs) // TODO
    // .then(verbatimEnvironments) // TODO
;

const environment = alt(anonym, env);


/** Comment
 *
 *  > % this is a comment`
 *
 * NOTE:
 *
 *  Q: When a line ends with a comment character like %,
 *     are spaces ignored at the beginning of the next line?
 *
 *  A: Yes; characters of category 10 are ignored at the
 *     beginning of every line, since every line starts in state N.
 *
 * We get this for free with ignoring the spaces
 */
export const comment: Parser<TeXComment> = commentSymbol
    .then(takeTillNewline)
    .skip(maybeNewline)
    .map(newTeXComment)
;

// Helpers
export const specialCharsDefault = {
    "'": true,
    "(": true,
    ")": true,
    ",": true,
    ".": true,
    "-": true,
    '"': true,
    "!": true,
    "^": true,
    "$": true,
    "&": true,
    "#": true,
    "{": true,
    "}": true,
    "%": true,
    "~": true,
    "|": true,
    "/": true,
    ":": true,
    ";": true,
    "=": true,
    "[": true,
    "]": true,
    "\\": true,
    "`": true,
    " ": true
};

export function isSpecialCharacter(char: string, specialChars?: { [k: string]: boolean }) {
    const chars = specialChars === undefined ? specialCharsDefault : specialChars;
    return chars.hasOwnProperty(char);
}


export const notTextDefault = {
    "$": true,
    "%": true,
    "\\": true,
    "{": true,
    "]": true,
    "}": true
};

export function isNotText(char: string, notText?: { [k: string]: boolean }) {
    const chars = notText === undefined ? notTextDefault : notText;
    return chars.hasOwnProperty(char);
}


// peekChar :: Parser (Maybe Char)
// peekChar = Just <$> (try $ lookAhead anyChar) <|> pure Nothing

// atEnd :: Parser Bool
// export const atEnd = Parjs.eof;

// takeTill :: (Char -> Bool) -> Parser Text
// takeTill p = T.pack <$> many (satisfy (not . p))

// Doubles
// export const floating = Parjs.float();


export const mathSymbol = string("$");


// whitespace.then(latexBlockParser).skip(whitespace),
// whitespace.result({})

/** Parser of a single 'LaTeX' constructor, no appending blocks.*/
export const latexBlockParser: Parser<LaTeX> = lazy(() => alt(
    alt(
        text              // <?> "text"
        , dolMath         // <?> "inline math ($)"
        // , comment         // <?> "comment"
        // , text2           // <?> "text2"
        // // , try environment // <?> "environment"
        // , command         // <?> "command"
        // , command         // <?> "command"
    )
    )
);

/**
 * Math
 */
export const dolMath = math("Dollar");

function math(t: MathType,
              sMath = "$",
              eMath = "$"): Parser<TeXMath> {
    return string(sMath)
        .then(latexBlockParser /*many*/)
        .skip(string(eMath))
        .map(str => newTeXMath(t, str))
        ;
}

//
// // Special commands (consisting of one char)
// export const special = Parjs.anyChar();


// x <- anyChar
// case x of
// '('  -> math Parentheses "\\)"
// '['  -> math Square      "\\]"
// '{'  -> lbrace
// '}'  -> rbrace
// '|'  -> vert
// '\\' -> lbreak
// _    -> commS [x]
