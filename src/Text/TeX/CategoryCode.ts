export const categories = {
    0: "Escape character", // \
    1: "Beginning of group", // {
    2: "End of group", // }
    3: "Math shift", // $
    4: "Alignment tab", // &
    5: "End of line", // [RETURN]
    6: "Parameter", // #
    7: "Superscript", // ^
    8: "Subscript", // _
    9: "Ignored character", // [NULL]
    10: "Space", // " "
    11: "Letter", // A, . . . , Z and a, . . . , z)
    12: "Other character", // none of the above or below
    13: "Active character", // ~
    14: "Comment character", // %
    15: "Invalid character", // [DEL]
};

// TODO character code can be changed at any time. A full parser must deal with that.

/**
 * TeXBook, Chapter 7: How TEX Reads What You Type, p. 37:
 *
 * > In the first place, it’s wise to have a precise idea of what your keyboard
 *   sends to the machine. There are 256 characters that TEX might encounter at
 *   each step, in a file or in a line of text typed directly on your terminal. These
 *   256 characters are classified into 16 categories numbered 0 to 15:
 */
export type CharacterCategory = keyof typeof categories;


export interface TeXChar {
    unicode: number;
    category: CharacterCategory;
}

export function convertToTeXChars(categoryMap: (unicode: number) => CharacterCategory, str: string): TeXChar[] {
    const chars: TeXChar[] = [];
    for (let i = 0; i < str.length; i++) {
        const unicode: number = str.charCodeAt(i);
        chars.push({
            unicode,
            category: categoryMap(unicode)
        });
        }
    return chars;
}
