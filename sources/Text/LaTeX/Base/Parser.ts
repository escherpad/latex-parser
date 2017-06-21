import {Parjs, ReplyKind} from "parjs";
import {MathType, MathTypeHaving, newTeXComment} from "./Syntax";
import {ParjsAction, ParjsBasicAction} from "parjs/dist/internal/implementation/action";
import {Issues, ParsingState} from "parjs/dist/internal/implementation";
import {ParjsParser} from "parjs/dist/internal";
import {ArrayHelpers} from "parjs/dist/internal/implementation/functions/helpers";

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
// module Text.LaTeX.Base.Parser (
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
//   , sourceLine
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


// Helpers

export const specialChars = {
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

export function isSpecialCharacter(char: string) {
    return specialChars.hasOwnProperty(char);
}

// peekChar :: Parser (Maybe Char)
// peekChar = Just <$> (try $ lookAhead anyChar) <|> pure Nothing

// atEnd :: Parser Bool
export const atEnd = Parjs.eof;

// takeTill :: (Char -> Bool) -> Parser Text
// takeTill p = T.pack <$> many (satisfy (not . p))

// Doubles
export const floating = Parjs.float();


const commentSymbol = Parjs.string("%");
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


class CommentParseAction extends ParjsAction {
    isLoud = true;
    expecting = `a comment line`;

    protected _apply(ps: ParsingState) {
        const inner = commentSymbol.q
            .then(Parjs.regexp(/[^\n]*/))
            .then(Parjs.any(
                Parjs.string("\n"),
                Parjs.result("")
            ).q);

        // let {position} = ps;
        // let i = 0;

        inner.apply(ps);

        return ps;

        // if (ps.isOk) {
        //     position = ps.position;
        //     ArrayHelpers.maybePush(arr, ps.value);
        //     i++;
        // }
        //
        // if (ps.atLeast(ReplyKind.HardFail)) {
        //     return;
        // }
        // if (i < minSuccesses) {
        //     ps.kind = i === 0 ? ReplyKind.SoftFail : ReplyKind.HardFail;
        //     return;
        // }
        // ps.value = arr;
        // // recover from the last failure.
        // ps.position = position;
        // ps.kind = ReplyKind.OK;
    }
}
export const comment = new ParjsParser(new CommentParseAction()).withName("comment");

export const mathSymbol = Parjs.string("$");

/**
 * Math
 *
 * TODO return TeXMath
 */
export const dolMath = latexBlock.many().between(
    Parjs.string(mathSymbol),
    Parjs.string(mathSymbol)
);

function math(t: MathType, string: sMath, string: eMath) {
    return latexBlock.many().between(
        Parjs.string(sMath),
        Parjs.string(eMath)
    );
}


// Special commands (consisting of one char)
export const special = Parjs.anyChar();


// x <- anyChar
// case x of
// '('  -> math Parentheses "\\)"
// '['  -> math Square      "\\]"
// '{'  -> lbrace
// '}'  -> rbrace
// '|'  -> vert
// '\\' -> lbreak
// _    -> commS [x]