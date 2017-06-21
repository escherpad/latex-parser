const Parjs = require("parjs").Parjs;
const _ = require("underscore");
//define our identifier. Starts with a letter, followed by a letter or digit. The `str` combinator stringifies what's an array of characters.
let ident = Parjs.asciiLetter.then(Parjs.digit.or(Parjs.asciiLetter).many()).str.q;

let inner = Parjs.anyChar.many().str;

//A parser that parses an opening of a tag.
let openTag = ident.between(Parjs.string("<"), Parjs.string(">"))
    .act((result, userState) => {
    if (userState && userState.hasOwnProperty("tags"))
        userState.tags.push({tag: result, content: []});
}).q;

let closeTag =
    ident.between(Parjs.string("</"), Parjs.string(">"))
        .must((result, userState) => {
            if (userState) {
                let last = userState.tags[userState.tags.length - 1];
                return result === last.tag;
            } else
                return true;
        })
        .act((result, userState) => {
            if (userState) {
                let topTag = userState.tags.pop();
                userState.tags[userState.tags.length - 1].content.push(topTag);
            }
        }).q;


let anyTag = ident.or(closeTag.or(openTag)).many().state.map(x => x.tags[0].content);

// console.log(JSON.stringify(anyTag.parse("<A>a</A><B>a4</B>", {tags: [{content: []}]}), null, 2));



console.log(JSON.stringify(
    comment.parse(`% u h h h`, {}), null, 2)
);

// console.log(JSON.stringify(
//     floating.parse("2", {tags: [{content: []}]}), null, 2)
// );
// console.log(JSON.stringify(
//     floating.parse(".2", {tags: [{content: []}]}), null, 2)
// );
//
