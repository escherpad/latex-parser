import {isString} from "../../../src/Utils";

/**
 * TODO Maarten: where does this list come from?! AFAIK LaTeX has just three modes: paragraph, math and LR
 *
 * LaTeX modes
 * @enum {string}
 */
export const modes = {
    LIST: "LIST",        // list of items
    MATH: "MATH",        // mathematical expressionLatex
    PICTURE: "PICTURE",  // picture
    TABLE: "TABLE",      // LaTeX tabular
    TEXT: "TEXT",        // general text
    VERTICAL: "VERTICAL" // vertical spacing
};

export type KirillMode = keyof typeof modes;
export function isKirillMode(x: any): x is KirillMode {
    return modes.hasOwnProperty(x);
}
export function mustBeKirillMode(x: any, msg?: string): KirillMode {
    if (!isKirillMode(x)) throw new Error(msg);
    return x;
}

/**
 */
export type LatexMode = "Paragraph" | "Math" | "LR";
export function isLatexMode(x: any): x is LatexMode {
    if (!isString(x))
        return false;

    switch (x) {
        case "Paragraph":
        case "Math":
        case "LR":
            return true;
        default:
            return false;
    }
}

//noinspection JSUnusedGlobalSymbols
export function mustBeLatexMode(x: any, msg?: string): LatexMode {
    if (!isLatexMode(x)) throw new Error(msg);
    return x;
}

export type ModeStates = { [mode: string]: boolean };