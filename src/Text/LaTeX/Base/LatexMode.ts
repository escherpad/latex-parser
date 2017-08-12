
/**
 */
import {isString} from "../../../Utils";

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