var P = require("parsimmon");

var JSONParser = P.createLanguage({
            // This is the main entry point of the parser: a full JSON value.
            value: r =>
        P.alt(
            r.object,
            r.array,
            r.string,
            r.number,
            r.null,
            r.true,
            r.false
        ).thru(parser => whitespace.then(parser)),

// The basic tokens in JSON, with optional whitespace afterward.
    lbrace: () => word('{'),
    rbrace: () => word('}'),
    lbracket: () => word('['),
    rbracket: () => word(']'),
    comma: () => word(','),
    colon: () => word(':'),

    // `.result` is like `.map` but it takes a value instead of a function, and
    // `.always returns the same value.
    null: () => word('null').result(null),
    true: () => word('true').result(true),
    false: () => word('false').result(false),

    // Regexp based parsers should generally be named for better error reporting.
    string: () =>
token(P.regexp(/"((?:\\.|.)*?)"/, 1))
    .map(interpretEscapes)
    .desc('string'),

    number: () =>
token(P.regexp(/-?(0|[1-9][0-9]*)([.][0-9]+)?([eE][+-]?[0-9]+)?/))
    .map(Number)
    .desc('number'),

    // Array parsing is just ignoring brackets and commas and parsing as many nested
    // JSON documents as possible. Notice that we're using the parser `json` we just
    // defined above. Arrays and objects in the JSON grammar are recursive because
    // they can contain any other JSON document within them.
    array: r =>
r.lbracket
    .then(r.value.sepBy(r.comma))
    .skip(r.rbracket),

    // Object parsing is a little trickier because we have to collect all the key-
    // value pairs in order as length-2 arrays, then manually copy them into an
    // object.
    pair: r =>
P.seq(r.string.skip(r.colon), r.value),

    object: r =>
r.lbrace
    .then(r.pair.sepBy(r.comma))
    .skip(r.rbrace)
    .map(pairs => {
    var object = {};
pairs.forEach(pair => {
    var [key, value] = pair;
object[key] = value;
});
return object;
}),
});


///////////////////////////////////////////////////////////////////////

var text = `\
{
  "id": "a thing\\nice\tab",
  "another property!"
    : "also cool"
  , "weird formatting is ok too........😂": 123.45e1,
  "": [
    true, false, null,
    "",
    " ",
    {},
    {"": {}}
  ]
}
`;

function prettyPrint(x) {
    var opts = {depth: null, colors: 'auto'};
    var s = util.inspect(x, opts);
    console.log(s);
}

var ast = JSONParser.value.tryParse(text);
prettyPrint(ast);