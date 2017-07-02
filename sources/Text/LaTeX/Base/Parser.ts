// <reference types="../../../../../../types/parsimmon.d.ts" />

import {
    alt, regexp,
    string, Parser, lazy, takeWhile,
    Success, Result, seqMap,
    ResultInterface,
    makeSuccess, Failure, seq
} from "parsimmon";

import {
    FixArg,
    LaTeX, mappend,
    MathType, MOptArg,
    newFixArg,
    newMOptArg,
    newOptArg,
    newTeXComm, newTeXComment, newTeXEnv, newTeXMath, OptArg, TeXArg, TeXComm,
    TeXComment,
    TeXEmpty, TeXEnv,
    TeXMath, TeXSeq
} from "./Syntax";
import {TeXRaw} from "./Syntax";
import {newTeXRaw} from "./Syntax";
import Parsimmon = require("parsimmon");
import {
    mconcat,
    mustBeNumber,
    mustNotBeUndefined
} from "../../../lib/Utils";

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

/**
 * Parser with 'Text' input and 'ParserConf' environment.
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
/**
 Returns the sorted set union of two arrays of strings. Note that if both
 arrays are empty, it simply returns the first array, and if exactly one
 array is empty, it returns the other one unsorted. This is safe because
 expectation arrays always start as [] or [x], so as long as we merge with
 this function, we know they stay in sorted order.
 */
function unsafeUnion(xs: any[], ys: any[]) {
    // Exit early if either array is empty (common case)
    const xn = xs.length;
    const yn = ys.length;
    if (xn === 0) {
        return ys;
    } else if (yn === 0) {
        return xs;
    }
    // Two non-empty arrays: do the full algorithm
    const obj: any = {};
    for (let i = 0; i < xn; i++) {
        obj[xs[i]] = true;
    }
    for (let j = 0; j < yn; j++) {
        obj[ys[j]] = true;
    }
    const keys = [];
    for (const k in obj) {
        if (obj.hasOwnProperty(k)) {
            keys.push(k);
        }
    }
    keys.sort();
    return keys;
}

function mergeReplies<T, U>(result: ResultInterface<T>, last?: ResultInterface<U>): ResultInterface<T> {
    if (!last) {
        return result;
    }
    if (result.furthest > last.furthest) {
        return result;
    }
    const expected = (result.furthest === last.furthest)
        ? unsafeUnion(result.expected, last.expected)
        : last.expected;
    return {
        status: result.status,
        index: result.index,
        value: result.value,
        furthest: last.furthest,
        expected: expected
    };
}

function manyTillAndMap<T, U, V>(manyOf: Parser<T>, till: Parser<U>, map: (acc: V, res: (T)) => V, initial: V) {
    return Parsimmon(function (input: string, i: number): Result<V> {
        let accum: V = initial;

        let j = 0;
        let result: ResultInterface<T> | undefined = undefined;

        // let lengthUntilEnd = -1;
        //
        // for (let o = i; o < input.length; o++) {
        //     const endCodonFound = till._(input, o);
        //     if (endCodonFound.status) {
        //         input = input.substring(0, o);
        //         lengthUntilEnd = mustBeNumber(endCodonFound.index);
        //         break;
        //     }
        // }
        // if (lengthUntilEnd < 0) return Parsimmon.makeFailure(i, "No end codon found: " + till);

        while (i < input.length) {
            const bigParse = manyOf._(input, i);
            result = mustNotBeUndefined(mergeReplies(bigParse, result));
            if (isNotOk(result)) {
                return result;
                // TODO fail? test
            }
            j++;
            const value: T = mustNotBeUndefined(result.value);

            accum = map(accum, value);

            i = mustBeNumber(result.index);
            const endCodonFound = till._(input, i);
            if (endCodonFound.status) {
                i = mustBeNumber(endCodonFound.index);
                break;
            }
        }
        // i = lengthUntilEnd;
        const result2: Success<V> = makeSuccess(i, accum);
        return mustBeOk(mergeReplies(result2, result));
    });
}
function manyTill<T, U>(manyOf: Parser<T>, till: Parser<U>) {
    return manyTillAndMap(manyOf, till, (a: T[], el: T) => a.concat([el]), <T[]>[]);

    // return Parsimmon(function (input: string, i: number): Result<T[]> {
    //     const accum: T[] = [];
    //
    //     let j = 0;
    //     let result: ResultInterface<T> | undefined = undefined;
    //
    //     let endCodonFound = till._(input, i);
    //     if (endCodonFound.status) {
    //         i = mustBeNumba(endCodonFound.index);
    //     }
    //     while (!endCodonFound.status) {
    //         const bigParse = manyOf._(input, i);
    //         result = mustNotBeUndefined(mergeReplies(bigParse, result));
    //         if (isNotOk(result)) {
    //             return result;
    //         }
    //         j++;
    //         const value: T = mustNotBeUndefined(result.value);
    //         accum.push(value);
    //         i = mustBeNumba(result.index);
    //         endCodonFound = till._(input, i);
    //         if (endCodonFound.status) {
    //             i = mustBeNumba(endCodonFound.index);
    //             break;
    //         }
    //     }
    //
    //     const result2: Success<T[]> = makeSuccess(i, accum);
    //     return mustBeOk(mergeReplies(result2, result));
    // });
}

function token(parser: Parser<string>): Parser<string> {
    return parser.skip(whitespace);
}

// Several parsers are just strings with optional whitespace.
function word(str: string): Parser<string> {
    return string(str).thru(token);
}


const lbrace = "{";
const rbrace = "}";
const lbracket = "[";
const rbracket = "]";
const comma = ",";
const colon = ":";

const openingBracket = string(lbracket);
const closingBracket = string(rbracket);
const isClosingbracket = (str: string) => str === (rbracket);

function takeAtLeastOneTill(till: (s: string) => boolean): Parser<string> {
    return Parsimmon((str, i) => {
        const firstChar = str.charAt(i);
        if (i >= str.length || till(firstChar)) {
            return Parsimmon.makeFailure(i, "text character");
        } else {
            const strz = [firstChar];
            i++;
            let char = str.charAt(i);
            while (!till(char) && i < str.length) {
                strz.push(char);
                i++;
                char = str.charAt(i);
            }
            return Parsimmon.makeSuccess(i, strz.join(""));
        }
    });
}

/** Text is a sequence on characters that are not non-text*/
// TODO use character codes
export const text = takeAtLeastOneTill(isNotText)
    .map(match => newTeXRaw(match))
;

/**
 * Text without stopping on ']'
 */
export const text2 = closingBracket.then(
    takeAtLeastOneTill(isNotText)
        .map(match => newTeXRaw(match))
);

const spaces: Parser<TeXRaw> = regexp(/ */)
    .map(newTeXRaw);


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
export const comment: Parser<TeXComment> =
    commentSymbol
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

export const notTextDefaultAndNotClosingBracket = Object.assign({"]": true}, notTextDefault);

export function isNotText(char: string, notText?: { [k: string]: boolean }) {
    const chars = notText === undefined ? notTextDefault : notText;
    return chars.hasOwnProperty(char);
}

//noinspection JSUnusedGlobalSymbols
export const mathSymbol = string("$");

export const commandSymbol = string("\\");

/**
 * Parser of a single 'LaTeX' block. Note: text stops on ']'; if the other parsers fail on the rest,
 * text2 handles it, starting with ']'
 */
export const latexBlockParser: Parser<LaTeX> = lazy(() => alt(
    alt(
        text              // <?> "text"
        , dolMath         // <?> "inline math ($)"
        , comment         // <?> "comment"
        , text2           // <?> "text2"
        , environment     // <?> "environment"
        , command         // <?> "command"
    )
    )
);

export const latexParser: Parser<LaTeX[]> = latexBlockParser.many();

const anonym = string(lbrace)
    .then(
        latexBlockParser.many()
    )
    .skip(string(rbrace));

export const env = Parsimmon(function (input: string, i: number): Result<TeXEnv> {
    const beginFound = string("\\begin")
        .then(string(lbrace))
        .then(spaces)
        .then(regexp(/[a-zA-Z]+/))  // envName
        .skip(spaces)
        .skip(string(rbrace))
        ._(input, i);
    if (isNotOk(beginFound))
        return beginFound;

    i = mustBeNumber(beginFound.index);
    const envName: string = beginFound.value;

    // TODO args

    return manyTill(latexBlockParser, string("\\end")
        .then(string(lbrace))
        .then(spaces)
        .then(string(envName))
        .then(spaces)
        .then(string(rbrace))
    ).map(latex => newTeXEnv(envName, latex))._(input, i);
});

export const environment = alt(anonym, env);

/**
 * Special commands (consisting of one char)
 */
export const specialChar = Parsimmon.test(isSpecialCharacter);

function isUppercaseAlph(c: string) {
    return c >= "A" && c <= "Z";
}

function isLowercaseAlph(c: string) {
    return c >= "a" && c <= "z";
}

export const endCmd = (c: string) => !isLowercaseAlph(c) && !isUppercaseAlph(c);

const openingBrace = string("{");
const closingBrace = string("}");

const isClosingBrace = (str: string) => str === ("}");

export const fixArg: Parser<FixArg> = openingBrace
    .then(
        manyTill(latexBlockParser, closingBrace)
    ).map(newFixArg)
;

export const optArg: Parser<MOptArg | OptArg> = openingBracket
    .then(
        manyTill(latexBlockParser, closingBracket)
    ).map(newOptArg);

export const cmdArg: Parser<TeXArg> = alt(
    fixArg,
    optArg
    // => newTeXArg(str)
);

/**
 * Command Arguments
 */
export const cmdArgs: Parser<TeXArg[] | undefined> = alt(
    string("{}").map(() => []),

    cmdArg.map(s => s).atLeast(0)
).map(e => e);


// cmdArgs = try (string "{}" >> return (Just []))
// <|> fmap Just (try $ many1 cmdArg)
// <|> return Nothing

// cmdArg :: Parser TeXArg
// cmdArg = do
// c <- char '[' <|> char '{'
// let e = case c of
// '[' -> "]"
// '{' -> "}"
// _   -> error "this cannot happen!"
// b <- mconcat <$> manyTill latexBlockParser (string e)
// case c of
// '[' -> return $ OptArg b
// '{' -> return $ FixArg b
// _   -> error "this cannot happen!"

/**
 * Command
 */
export const command: Parser<TeXComm | TeXEmpty> = alt(
    commandSymbol.then(Parsimmon.eof).map(() => {
        return {};
    }),

    seqMap(
        commandSymbol,
        alt(specialChar, takeTill(endCmd)),
        cmdArgs,

        function (ignored, name, argz) {
            return argz !== undefined ? newTeXComm(name, ...argz) : newTeXComm(name);
        }
    )
).map(res => {
    return res;
});

/**
 * Math
 */
export const dolMath = math("Dollar");

function math(t: MathType,
              sMath = "$",
              eMath = "$"): Parser<TeXMath> {
    return string(sMath)
        .then(latexBlockParser.many())
        .skip(string(eMath))
        .map(str => newTeXMath(t, str))
        ;
}

export function isOk<T>(parse?: ResultInterface<T>): parse is Success<T> {
    return parse !== undefined && parse.status === true;
}

export function isNotOk<T>(parse?: any): parse is Failure {
    return parse !== undefined && parse.status === false;
}
export function mustBeOk<T>(parse?: ResultInterface<T>): Success<T> {
    if (!isOk(parse)) throw new Error("Expected parse to be success: " + JSON.stringify(parse));
    return parse;
}