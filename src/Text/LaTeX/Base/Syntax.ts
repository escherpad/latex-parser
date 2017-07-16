import {isArray, isNumber} from "../../../Utils";
import {convertToTeXChars, convertToTeXCharsDefault, defaultCategories, TeXChar} from "../../TeX/CategoryCode";

/** LaTeX syntax description in the definition of the 'LaTeX' datatype.
 If you want to add new commands or environments not defined in
 the library, import this module and use 'LaTeX' data constructors.
 module Text.LaTeX.Base.Syntax
 */

/**
 ( -- * @LaTeX@ datatype
 Measure (..)
 , MathType (..)
 , LaTeX (..)
 , TeXArg (..)
 , (<>)

 -- * Escaping reserved characters
 , protectString
 , protectText

 -- * Syntax analysis
 , matchCommand
 , lookForCommand
 , matchEnv
 , lookForEnv
 , texmap
 , texmapM

 -- ** Utils
 , getBody
 , getPreamble
 ) where

 import Data.Text (Text,pack)
 import qualified Data.Text
 import Data.Monoid
 #if MIN_VERSION_base(4,9,0)
 import qualified Data.Semigroup as Semigroup
 #endif
 import Data.String
 import Control.Applicative
 import Control.Monad (replicateM)
 import Data.Functor.Identity (runIdentity)
 import Data.Data (Data)
 import Data.Typeable
 import GHC.Generics (Generic)
 import Test.QuickCheck
 */
/**
 Measure units defined in LaTeX. Use 'CustomMeasure' to use commands like 'textwidth'.

 For instance:

 > rule Nothing (CustomMeasure linewidth) (Pt 2)

 This will create a blac box (see 'rule') as wide as the text and two points tall.
 */
export type Measure = BuiltInMeasure | CustomMeasure;
// deriving (
// Data
// Eq
// Generic,
// Show,
// Typeable

export function isMeasure(x: any): x is Measure {
    return isBuiltInMeasure(x) || isCustomMeasure(x);
}

export const measureTypes = {
    "pt": true,                    // A point is 1/72.27 inch, that means about 0.0138 inch or 0.3515 mm.
    "mm": true,                    // Millimeter.
    "cm": true,                    // Centimeter.
    "in": true,                    // Inch.
    "ex": true,                    // The height of an \"x\" in the current font.
    "em": true,                    // The width of an \"M\" in the current font.
};

export type MeasureType = keyof typeof measureTypes;

export function isMeasureType(x: any): x is MeasureType {
    return measureTypes.hasOwnProperty(x);
}

export interface BuiltInMeasure {
    type: MeasureType;
    value: number;
}

export function isBuiltInMeasure(x: any): x is BuiltInMeasure {
    return isMeasureType(x.type) && isNumber(x.value);
}

/**
 * You can introduce a 'LaTeX' expression as a measure.
 */
export interface CustomMeasure {
    expression: LaTeXRaw;
}

export function isCustomMeasure(x: any): x is CustomMeasure {
    return isLaTeXBlock(x.expression);
}

/** Different types of syntax for mathematical expressions.*/
export const mathTypes = {
    Parentheses: "Parentheses",
    Square: "Square",
    Dollar: "Dollar"
};
export type MathType = keyof typeof mathTypes;

// deriving (Data, Eq, Generic, Show, Typeable)
export function isMathType(x: any): x is MathType {
    if (x === undefined)
        return false;
    else switch (x) {
        case "Parentheses":
        case "Square":
        case "Dollar":
            return true;
        default:
            return false;
    }
}

export interface NameHaving {
    name: string;
}

export function isNameHaving(x: any, name?: string): x is NameHaving {
    return x !== undefined && (name === undefined
                ? typeof x.name === "string"
                : name === x.name
        );
}

export interface CharCodeHaving {
    charCode: number;
}

export interface CharacterCatergiesHaving {
    characterCategories: TeXChar[];
}

export interface TextHaving {
    text: string;
}

export function isTextHaving(x: any): x is TextHaving {
    return x !== undefined && typeof x.text === "string";
}


export interface LaTeXHaving {
    latex: LaTeXTxt[];
}
export type MultipleLaTeXHaving = LaTeXHaving;

export function isLaTeXHaving(x: any): x is LaTeXHaving {
    return x !== undefined && isArray(x.latex);
}

export interface MathTypeHaving {
    type: MathType;
}

export interface ArgumentHaving {
    arguments: TeXArg[];
}

export function isArgumentHaving(x: any): x is ArgumentHaving {
    return x.arguments instanceof Array;
}


/**
 * Types of @LaTeX@ blocks.
 */
export type LaTeX = LaTeXRaw | LaTeXNoRaw;

export type LaTeXRaw = TeXBuildingBlocks | TeXRaw;

/**
 * Raw TeX string converted to @TeXChar@s.
 */
export type LaTeXNoRaw = TeXBuildingBlocks | TeXChar;

export type TeXBuildingBlocks =
    TeXComm |
    TeXEnv |
    TeXMath |
    TeXLineBreak |
    SubOrSuperScript |
    TeXBraces |
    TeXComment;

// type LaTeXExtended = LaTeXRawString |
//     TeXEmpty;


// deriving (Data, Eq, Generic, Show, Typeable)

export interface TypeHaving {
    type: string;
}

export type TypeTeXSeq = "TeXSeq";
export const typeTeXSeq: TypeTeXSeq = "TeXSeq"; //noinspection JSUnusedGlobalSymbols
export interface TypeHavingTeXSeq extends TypeHaving {
    type: TypeTeXSeq;
}

export type TypeTeXEnv = "TeXEnv";
export const typeTeXEnv: TypeTeXEnv = "TeXEnv";
export interface TypeHavingTeXEnv extends TypeHaving {
    type: TypeTeXEnv;
}

export type TypeTeXBraces = "TeXBraces";
export const typeTeXBraces: TypeTeXBraces = "TeXBraces";
export interface TypeHavingTeXBraces extends TypeHaving {
    type: TypeTeXBraces;
}

export type TypeTeXComment = "TeXComment";
export const typeTeXComment: TypeTeXComment = "TeXComment";
export interface TypeHavingTeXComment extends TypeHaving {
    type: TypeTeXComment;
}

export type TypeTeXRaw = "TeXRaw";
export const typeTeXRaw: TypeTeXRaw = "TeXRaw";
export interface TypeHavingTeXRaw extends TypeHaving {
    type: TypeTeXRaw;
}

// export type TypeTeXChar = "TeXChar";
// export const typeTeXChar: TypeTeXChar = "TeXChar";
// export interface TypeHavingTeXChar extends TypeHaving {
//     type: TypeTeXChar;
// }

export type TypeTeXComm = "TeXComm" | TypeTeXCommS;
export const typeTeXComm: TypeTeXComm = "TeXComm";
export interface TypeHavingTeXComm extends TypeHaving {
    type: TypeTeXComm;
}

export type TypeTeXCommS = "TeXCommS";
export const typeTeXCommS: TypeTeXCommS = "TeXCommS"; //noinspection JSUnusedGlobalSymbols
export interface TypeHavingTeXCommS extends TypeHaving {
    type: TypeTeXCommS;
}

export type TeXRaw = TextHaving & TypeHavingTeXRaw & CharacterCatergiesHaving; // Raw text.

export type TeXComment = TextHaving & TypeHavingTeXComment; // Comments.
export type TeXComm = NameHaving & ArgumentHaving & TypeHavingTeXComm;
export type TeXEnv = MultipleLaTeXHaving & NameHaving & ArgumentHaving & TypeHavingTeXEnv;
export type TeXMath = MultipleLaTeXHaving & MathTypeHaving; // Mathematical expressions.
export type TeXBraces = LaTeXHaving & TypeHavingTeXBraces;
export type SubOrSuperSymbol = "^" | "_";
export interface SubOrSuperScript {
    type: SubOrSuperSymbol;
    arguments?: TeXArg[];
}

/**
 An expression between braces.
 Line break command.
 */
export interface TeXLineBreak {
    measure?: Measure;
    noNewPage: boolean;
}

/** Sequencing of 'LaTeX' expressions.*/
// export interface TeXSeq {
//     head: LaTeX;
//     tail: LaTeX;
//     type: TypeTeXSeq;
// }

// /**
//  An empty block.
//  /Neutral element/ of '<>'.
//  */
// export interface TeXEmpty {
// }

/**
 * When rendering, no space or @{}@ will be added at
 * the end.
 */
export interface TeXCommS extends TeXComm {
    // Must be empty, but can't express it in the type :(
    // arguments: [];
    type: TypeTeXCommS;
}


// An argument for a 'LaTeX' command or environment.
export type TeXArg = FixArg |
    OptArg |
    MOptArg |
    SymArg |
    MSymArg |
    ParArg |
    MParArg;


// deriving (Data, Eq, Generic, Show, Typeable)


export type FixArg = MultipleLaTeXHaving & TypeHavingFixArg; // Fixed argument.

export type OptArg = LaTeXHaving & TypeHavingOptArg; // Optional argument.
export type SymArg = LaTeXHaving & TypeHavingSymArg; // An argument enclosed between @\<@ and @\>@.
export type ParArg = LaTeXHaving & TypeHavingParArg; // An argument enclosed between @(@ and @)@.
export type MOptArg = MultipleLaTeXHaving & TypeHavingMOptArg; // Multiple optional argument.
export type MSymArg = MultipleLaTeXHaving & TypeHavingMSymArg; // Version of 'SymArg' with multiple options.
export type MParArg = MultipleLaTeXHaving & TypeHavingMParArg; // Version of 'ParArg' with multiple options.

export interface TypeHavingFixArg extends TypeHaving {type: "FixArg";
}
export interface TypeHavingOptArg extends TypeHaving {type: "OptArg";
}
export interface TypeHavingMOptArg extends TypeHaving {type: "MOptArg";
}
export interface TypeHavingSymArg extends TypeHaving {type: "SymArg";
}
export interface TypeHavingMSymArg extends TypeHaving {type: "MSymArg";
}
export interface TypeHavingParArg extends TypeHaving {type: "ParArg";
}
export interface TypeHavingMParArg extends TypeHaving {type: "MParArg";
}

//
// Monoid instance for 'LaTeX'.
//
//
// export const mempty: TeXEmpty = {};
//
// /** Method 'mappend' is strict in both arguments (except in the case when the first argument is 'TeXEmpty').*/
// export function mappend(x: LaTeX, y: LaTeX): typeTeXSeq {
//     if (isTeXEmpty(y))
//         return x;
//     else if (isTeXEmpty(x))
//         return y;
//     // else if (isTeXSeq(x))
//     //     return {
//     //         head: x.head,
//     //         tail: mappend(x.tail, y)
//     //     };
//     else
//         return {
//             head: x,
//             tail: y
//         };
// }

/** Method 'fromString' escapes LaTeX reserved characters using 'protectString'.*/
export const fromStringLaTeX = (x: string) => newTeXRaw(protectString(x));

/** Escape LaTeX reserved characters in a 'String'.*/
export const protectString = (s: string) => {
    const newString = [];
    for (let i = 0; i < s.length; i++)
        newString.push(protectChar(s.charAt(i)));
    return newString.join();
};

// -- | Escape LaTeX reserved characters in a 'Text'.
//     protectText :: Text -> Text
// protectText = Data.Text.concatMap (fromString . protectChar)

export function protectChar(c: string): string {
    switch (c) {
        case "#":
            return "\\#";
        case "$":
            return "\\$";
        case "%":
            return "\\%";
        case "^":
            return "\\^{}";
        case "&":
            return "\\&";
        case "{":
            return "\\{";
        case "}":
            return "\\}";
        case "~":
            return "\\~{}";
        case "\\":
            return "\\textbackslash{}";
        case "_":
            return "\\_{}";
        default:
            return c;
    }
}


//
// Syntax analysis
//


// /** Look into a 'LaTeX' syntax tree to find any call to the command with
//  the given name. It returns a list of arguments with which this command
//  is called.
//
//  > lookForCommand = (fmap snd .) . matchCommand . (==)
//
//  If the returned list is empty, the command was not found. However,
//  if the list contains empty lists, those are callings to the command
//  with no arguments.
//
//  For example
//
//  > lookForCommand "author" l
//
//  would look for the argument passed to the @\\author@ command in @l@.
//  */
// export const lookForCommand = (commandName: string, latex: LaTeX): TeXArg[][] =>
//     matchCommand(s => s === commandName, latex).map(snd);
//
// /** Traverse a 'LaTeX' syntax tree and returns the commands (see 'TeXComm' and
//  'TeXCommS') that matches the condition and their arguments in each call.*/
// export const matchCommand = (f: ((s: string) => boolean), l: LaTeX): [string, TeXArg[]][] => {
//     // if (isTeXSeq(l))
//     //     return (matchCommand(f, l.head)).concat(matchCommand(f, l.tail));
//
//     if (isTeXCommS(l))
//         return f(l.name) ? [[l.name, []]] : [];
//
//     if (isTeXComm(l)) {
//         const xs: [string, TeXArg[]][] = concatMap(l.arguments, arg => matchCommandArg(f, arg));
//         if (f(l.name)) {
//             const a: [string, TeXArg[]][] = [[l.name, l.arguments]];
//             return a.concat(xs);
//         } else {
//             return xs;
//         }
//     }
//
//     if (isTeXMath(l) || isTeXBraces(l))
//         return matchCommand(f, l.latex);
//
//     return [];
// };
//
// export const matchCommandArg = (f: ((string: string) => boolean), l: TeXArg): [string, TeXArg[]][] => {
//     if (isMultipleLaTeXHaving(l)) {
//         const res: [string, TeXArg[]][] = [].concat.apply([], mustBeArray(l.latex).map(latex => matchCommand(f, latex)));
//         return res;
//     }
//     else {
//         return matchCommand(f, l.latex);
//     }
// };


// /** Similar to 'lookForCommand', but applied to environments.
//  It returns a list with arguments passed and content of the
//  environment in each call.
//
//  > lookForEnv = (fmap (\(_,as,l) -> (as,l)) .) . matchEnv . (==)
//  */
// const compressEnv = (([ignored, as, l]: [any, TeXArg[], LaTeX]): [TeXArg[], LaTeX] => [as, l]);
// export const lookForEnv = (s: string, l: LaTeX): [TeXArg[], LaTeX][] => {
//     return matchEnv(str => str === s, l).map(compressEnv);
// };

// /** Traverse a 'LaTeX' syntax tree and returns the environments (see
//  'TeXEnv') that matches the condition, their arguments and their content
//  in each call.*/
// export const matchEnv = (f: ((s: string) => boolean), l: LaTeX): [string, TeXArg[], LaTeX][] => {
//     if (isTeXComm(l)) {
//         return concatMap(
//             l.arguments,
//             (a: TeXArg): [string, TeXArg[], LaTeX][] => matchEnvArg(f, a)
//         );
//     }
//     // else if (isTeXSeq(l))
//     //     return matchEnv(f, l.head).concat(matchEnv(f, l.tail));
//     else if (isTeXEnv(l)) {
//         const tail: [string, TeXArg[], LaTeX][] = concatMap(l.arguments, (a: TeXArg) => matchEnvArg(f, a))
//             .concat(matchEnv(f, l.latex));
//         if (f(l.name)) {
//             const head: [string, TeXArg[], LaTeX][] = [[l.name, l.arguments, l.latex]];
//             const concat: [string, TeXArg[], LaTeX][] = head.concat(tail);
//             return concat;
//         } else {
//             return tail;
//         }
//     }
//     else if (isTeXMath(l) || isTeXBraces(l))
//         return matchEnv(f, l.latex);
//     else {
//         return [];
//     }
// };
//
// export const matchEnvArg = (f: ((s: string) => boolean), l: TeXArg): [string, TeXArg[], LaTeX][] => {
//     if (isMultipleLaTeXHaving(l)) {
//         return concatMap(l.latex, (latex: LaTeX): [string, TeXArg[], LaTeX][] => matchEnv(f, latex));
//     } else {
//         return matchEnv(f, l.latex);
//     }
// };


//  /** The function 'texmap' looks for subexpressions that match a given
//     condition and applies a function to them.
//
//   > texmap c f = runIdentity . texmapM c (pure . f)
//   */
// export const texmap = (condition: (l: LaTeX) => boolean,
//      f: ((l: LaTeX) => LaTeX), // Function to apply when the condition matches.
//      l: LaTeX
//  ): LaTeX => {
//  runIdentity(texmapM condition (pure . f))
//
//  /** Version of 'texmap' where the function returns values in a 'Monad'.
//  texmapM :: (Applicative m, Monad m)
//  => (LaTeX -> Bool) // Condition.
//  -> (LaTeX -> m LaTeX) // Function to apply when the condition matches.
//  ->  LaTeX -> m LaTeX
//  texmapM c f = go
//  where
//  go l@(TeXComm str as)  = if c l then f l else TeXComm str <$> mapM go' as
//  go l@(TeXEnv str as b) = if c l then f l else TeXEnv str <$> mapM go' as <*> go b
//  go l@(TeXMath t b)     = if c l then f l else TeXMath t <$> go b
//  go l@(TeXBraces b)     = if c l then f l else TeXBraces <$> go b
//  go l@(TeXSeq l1 l2)    = if c l then f l else liftA2 TeXSeq (go l1) (go l2)
//  go l = if c l then f l else pure l
//  --
//  go' (FixArg  l ) = FixArg  <$> go l
//  go' (OptArg  l ) = OptArg  <$> go l
//  go' (MOptArg ls) = MOptArg <$> mapM go ls
//  go' (SymArg  l ) = SymArg  <$> go l
//  go' (MSymArg ls) = MSymArg <$> mapM go ls
//  go' (ParArg  l ) = ParArg  <$> go l
//  go' (MParArg ls) = MParArg <$> mapM go ls

// /**
//  * Extract the content of the 'document' environment, if present.
//  */
// export const getBody = (l: LaTeX): LaTeX | undefined => {
//     const env = lookForEnv("document", l);
//     return env.length > 0 ? env[0] : undefined;
// };

// /** Extract the preamble of a 'LaTeX' document (everything before the 'document'
//  environment). It could be empty.*/
// export const getPreamble = (l: LaTeX): LaTeX => {
//     if (isTeXEnv(l, "document"))
//         return mempty;
//
//     // else if (isTeXSeq(l))
//     //     return mappend(
//     //         getPreamble(l.head),
//     //         getPreamble(l.tail)
//     //     );
//
//     else
//         return l;
// };


// ---------------------------------------
//    -- LaTeX Arbitrary instance
// TODO? with generators?

// arbitraryChar :: Gen Char
// arbitraryChar = elements $
//     ['A'..'Z']
// ++ ['a'..'z']
// ++ "\n-+*/!\"$%&(){}^_.,:;'#@<>?\\ "

/** Utility for the instance of 'LaTeX' to 'Arbitrary'.
 --   We generate a short sequence of characters and
 --   escape reserved characters with 'protectText'.
 arbitraryRaw :: Gen Text
 arbitraryRaw = do
 n <- choose (1,20)
 protectText . pack <$> replicateM n arbitraryChar

 /** Generator for names of command and environments.
 --   We use only alphabetical characters.
 arbitraryName :: Gen String
 arbitraryName = do
 n <- choose (1,10)
 replicateM n $ elements $ ['a' .. 'z'] ++ ['A' .. 'Z']

 instance Arbitrary Measure where
 arbitrary = do
 n <- choose (0,5)
 let f = [Pt,Mm,Cm,In,Ex,Em] !! n
 f <$> arbitrary

 instance Arbitrary LaTeX where
 arbitrary = do
 -- We give more chances to 'TeXRaw'.
 -- This results in arbitrary 'LaTeX' values
 -- not getting too large.
 n <- choose (0,16 :: Int)
 case n of
 0 -> pure TeXEmpty
 1 -> do m <- choose (0,5)
 TeXComm <$> arbitraryName <*> vectorOf m arbitrary
 2 -> TeXCommS <$> arbitraryName
 3 -> do m <- choose (0,5)
 TeXEnv <$> arbitraryName <*> vectorOf m arbitrary <*> arbitrary
 4 -> do m <- choose (0,2)
 let t = [Parentheses,Square,Dollar] !! m
 TeXMath <$> pure t <*> arbitrary
 5 -> TeXLineBreak <$> arbitrary <*> arbitrary
 6 -> TeXBraces <$> arbitrary
 7 -> TeXComment <$> arbitraryRaw
 8 -> TeXSeq <$> arbitrary <*> arbitrary
 _ -> TeXRaw <$> arbitraryRaw

 instance Arbitrary TeXArg where
 arbitrary = do
 n <- choose (0,6 :: Int)
 case n of
 0 -> OptArg <$> arbitrary
 1 -> do m <- choose (1,5)
 MOptArg <$> vectorOf m arbitrary
 2 -> SymArg <$> arbitrary
 3 -> do m <- choose (1,5)
 MSymArg <$> vectorOf m arbitrary
 4 -> ParArg <$> arbitrary
 5 -> do m <- choose (1,5)
 MParArg <$> vectorOf m arbitrary
 _ -> FixArg <$> arbitrary
 */


//
// type guards
//


export function isTypeHaving(x: any, ...anyOfTypes: string[]): x is TypeHaving {
    return anyOfTypes.length === 0 ? typeof x.type === "string" : anyOfTypes.some(type => x.type === type);
}

export function isLaTeXBlock(x: any): x is (LaTeXRaw | LaTeXNoRaw) {
    return isLaTeXRaw(x) || isLaTeXNoRaw(x); // || isTeXSeq(x);
}

export function isLaTeXNoRaw(x: any): x is LaTeXNoRaw {
    return isTeXEmpty(x)
        || isTeXChar(x)
        || isTeXComm(x)
        || isTeXEnv(x)
        || isTeXMath(x)
        || isTeXLineBreak(x)
        || isTeXBraces(x)
        || isTeXComment(x);
}

export function isLaTeXRaw(x: any): x is LaTeXRaw {
    return isTeXEmpty(x)
        || isTeXRaw(x)
        || isTeXComm(x)
        || isTeXEnv(x)
        || isTeXMath(x)
        || isTeXLineBreak(x)
        || isTeXBraces(x)
        || isTeXComment(x);
}

export function isTeXRaw(x: any): x is TeXRaw {
    return x !== undefined
        && x.type !== undefined
        && isTextHaving(x) && isTypeHaving(x, typeTeXRaw);
}

export function isTeXChar(x: any): x is TeXChar {
    return x !== undefined
        // && x.charCode !== undefined
        && typeof x === "number";
}

export function isTeXComm(x: any): x is TeXComm {
    return isNameHaving(x)
        && isArgumentHaving(x)
        && isTypeHaving(x, typeTeXComm, typeTeXCommS)
        ;
}

export function isTeXCommS(x: any): x is TeXCommS {
    return isTeXComm(x) && x.arguments.length === 0;
}

//noinspection JSUnusedLocalSymbols
export function isTeXEnv(x: any, name?: string): x is TeXEnv {
    return isTypeHaving(x, typeTeXEnv)
        ;
}

export function isTeXMath(x: any): x is TeXMath {
    return isLaTeXHaving(x) && isTypeHaving(x) && isMathType(x.type);
}

export function isTeXLineBreak(x: any): x is TeXLineBreak {
    return x !== undefined && typeof x.noNewPage === "boolean" && (x.measure === undefined || isMeasure(x.measure));
}

export function isTeXBraces(x: any): x is TeXBraces {
    return isLaTeXHaving(x) && isTypeHaving(x, typeTeXBraces);
}

//noinspection JSUnusedGlobalSymbols
export function isFixArg(x: any): x is FixArg {
    return isTypeHaving(x, "FixArg");
}

//noinspection JSUnusedGlobalSymbols
export function isOptArg(x: any): x is OptArg {
    return isTypeHaving(x, "OptArg");
}

export function isTeXComment(x: any): x is TeXComment {
    return isTextHaving(x) && isTypeHaving(x, typeTeXComment);
}


// export function isTeXSeq(x: any): x is TeXSeq {
//     return x !== undefined && x.head && x.tail
//         && isTypeHaving(x, typeTeXSeq)
//         ;
// }

export function isTeXEmpty(e: any) {
    return e !== undefined && Object.keys(e).length === 0;
}

//
// constructors
//
export type LaTeXTxt = LaTeXRaw | TeXChar;

export function newFixArg(l: LaTeXTxt[]): FixArg {
    return {type: "FixArg", latex: l};
}
export function newOptArg(l: LaTeXTxt[]): MOptArg | OptArg {
    return l.length === 1 ? {type: "OptArg", latex: l} : {type: "MOptArg", latex: l};
}
//noinspection JSUnusedGlobalSymbols
export function newSymArg(l: LaTeXRaw): SymArg {
    return {type: "SymArg", latex: [l]};
}
//noinspection JSUnusedGlobalSymbols
export function newParArg(l: LaTeXRaw): ParArg {
    return {type: "ParArg", latex: [l]};
}
//noinspection JSUnusedGlobalSymbols
export function newMOptArg(l: LaTeXRaw[]): MOptArg {
    return {type: "MOptArg", latex: l};
}
//noinspection JSUnusedGlobalSymbols
export function newMSymArg(l: LaTeXRaw[]): MSymArg {
    return {type: "MSymArg", latex: l};
}
//noinspection JSUnusedGlobalSymbols
export function newMParArg(l: LaTeXRaw[]): MParArg {
    return {type: "MParArg", latex: l};
}

/**
 Constructor for commands with no arguments.
 */ //noinspection JSUnusedGlobalSymbols
export function newCommandS(name: string): TeXCommS {
    return {
        name,
        arguments: [],
        type: typeTeXCommS
    };
}


export function newTeXRaw(text: string): TeXRaw {
    return {
        text,
        type: typeTeXRaw,
        characterCategories: convertToTeXCharsDefault(text)
    };
}

export function newTeXMath(type: MathType, latex: LaTeXRaw[]): TeXMath {
    return {
        latex,
        type
    };
}

//noinspection JSUnusedGlobalSymbols
export function newTeXBraces(latex: LaTeXRaw): TeXBraces {
    return {
        latex: [latex],
        type: typeTeXBraces
    };
}

export const newTeXMathDol: (l: LaTeXRaw[]) => TeXMath = newTeXMath.bind(undefined, "Dollar");

export function newTeXComment(text: string): TeXComment {
    return {
        text,
        type: typeTeXComment
    };
}


/** Constructor for commands.
 * First argument is the name of the command.
 * Second, its arguments.*/
export function newTeXComm(name: string, ...args: TeXArg[]): TeXComm {
    return {
        name,
        arguments: args,
        type: typeTeXComm
    };
}

export function newSubOrSuperScript(type: SubOrSuperSymbol, args?: TeXArg[]): SubOrSuperScript {
    return {
        type,
        arguments: args
    };
}

/**
 * Constructor for environments.
 * First argument is the name of the environment.
 * Second, its content.
 * Third, its arguments.
 */
export function newTeXEnv(name: string, latex: LaTeXRaw[], ...args: TeXArg[]): TeXEnv {
    return {
        name,
        latex,
        arguments: args,
        type: typeTeXEnv
    };
}

// export function cleanTexRaw(tex: LaTeX[]): LaTeXNoRaw[] {
//     let newArray: LaTeXNoRaw[] = [];
//     tex.forEach(tex => {
//         if (isTeXRaw(tex))
//             newArray = newArray.concat(convertToTeXChars(
//                 uni => defaultCategories(uni),
//                 tex.text
//             ));
//         else if (isTeXComm(tex)) {
//             const newArgs: LaTeXNoRaw[] = cleanTexRaw(tex.arguments);
//             newArray.push(newTeXComm(tex.name, newArgs));
//         }
//         else if (isTeXEnv(tex)) newArray.push(newTeXEnv());
//         else if (isTeXMath(tex)) newArray.push(newTeXMath());
//         else if (isTeXBraces(tex)) newArray.push(newTeXBraces());
//         else if (isTeXComment(tex)) newArray.push(newTeXComment());
//         else newArray.push(tex);
//     });
//     return newArray;
// }
//
// export function renderToTex(tex: LaTeX): string {
//     // TODO
//     return "";
// }