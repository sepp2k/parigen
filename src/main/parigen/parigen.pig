grammar: rule*;
rule: "token"? ID ":" expression ";";
expression: sequence ("|" sequence)*;
sequence: quantifiedExpression+;
quantifiedExpression: primaryExpression quantifier?;
quantifier: "*" | "+" | "?";
primaryExpression:
    ID |
    STRING_LIT |
    CHARACTER_CLASS |
    "." |
    "(" expression ")"
    ;
token CHARACTER_CLASS: "[" ("\\" . | [^\]])* "]";
token ID: [a-zA-Z_][a-zA-Z_0-9]*;
token STRING_LIT: "\"" ("\\" . | [^"\\])* "\"";
token WS: [ \t\r\n]+;