var fs = require("fs");
var process = require("process");
var path = require("path");
var lexerFile = path.resolve(process.argv[2]);
var lexerMod = require(lexerFile);
var lexer = new lexerMod.Lexer(fs.readFileSync(process.argv[3], "UTF-8"));
while (lexer.hasNext()) {
    var token = lexer.nextToken();
    console.log(lexerMod.TokenType[token.kind] + " (" + token.fromIndex + " - " + token.toIndex + "): " + token.value);
}