/**
 * TeXBook, Chapter 7: How TEX Reads What You Type, p. 37:
 *
 * > In the first place, it’s wise to have a precise idea of what your keyboard
 *   sends to the machine. There are 256 characters that TEX might encounter at
 *   each step, in a file or in a line of text typed directly on your terminal. These
 *   256 characters are classified into 16 categories numbered 0 to 15:
 */
export type CharacterCategory =
    0 | // "Escape character", // \
    1 | // "Beginning of group", // {
    2 | // "End of group", // }
    3 | // "Math shift", // $
    4 | // "Alignment tab", // &
    5 | // "End of line", // [RETURN]
    6 | // "Parameter", // #
    7 | // "Superscript", // ^
    8 | // "Subscript", // _
    9 | // "Ignored character", // [NULL]
    10 | // "Space", // " "
    11 | // "Letter", // A, . . . , Z and a, . . . , z)
    12 | // "Other character", // none of the above or below
    13 | // "Active character", // ~
    14 | // "Comment character", // %
    15  // "Invalid character", // [DEL]
    ;

// TODO character code can be changed at any time. A full parser must deal with that.

export const defaultCategories = (char: string,
                                  // strictAboutLetters: boolean = false // TODO
): CharacterCategory => {
    switch (char) {
        case "\\" :
            return 0;
        case "{":
            return 1;
        case "}":
            return 2;
        case "$":
            return 3;
        case "&":
            return 4;
        case "\r":
            return 5;
        case "#":
            return 6;
        case "^":
            return 7;
        case "_":
            return 8;
        case "\0":
            return 9;
        case " ":
            return 10;
        case "~":
            return 13;
        case "%":
            return 14;
        case "\d":
            return 15;
        default:
            return 11; // Assume letter
        // if (
        //     (unicodePointer >= 97 && unicodePointer <= 122) // a-z
        //     ||
        //     (unicodePointer >= 65 && unicodePointer <= 90) // A-Z
        // ) {
        //     return 11; // strictly [a-zA-Z]
        // } else if() {
        // }
        // return 12; // Other character
    }
};

export interface TeXChar {
    string: string;
    category: CharacterCategory;
}

export function convertToTeXCharsDefault(str: string): TeXChar[] {
    return convertToTeXChars(defaultCategories, str);
}

export function convertToTeXChars(categoryMap: (unicode: string) => CharacterCategory,
                                  str: string): TeXChar[] {
    const chars: TeXChar[] = [];
    for (let i = 0; i < str.length; i++) {
        const charAt = str.charAt(i);
        chars.push({
            string: charAt,
            category: categoryMap(charAt)
        });
    }
    return chars;
}
