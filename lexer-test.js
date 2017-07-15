let fs = require("fs");
let process = require("process");
let lexer = new Lexer(fs.readFileSync(process.argv[2], "UTF-8"));
while (lexer.hasNext()) {
    let token = lexer.nextToken();
    console.log(TokenType[token.kind] + " (" + token.fromIndex + " - " + token.toIndex + ")");
}