package parigen

import scala.util.parsing.combinator._

object Parser extends RegexParsers {
    def grammar = rule.* ^^ {
        rules => ast.Grammar(rules.toMap)
    }

    def rule =
        "token".? ~ (ID <~ ":") ~ expression <~ ";" ^^ {
            case tokenOpt ~ name ~ exp =>
                name -> ast.Rule(name, ast.RuleFlags(tokenRule = tokenOpt.isDefined), exp)
        }

    def expression: Parser[ast.Expression] =
        rep1sep(sequence, "|") ^^ {
            exps => exps.reduce(ast.Or)
        }

    def sequence =
        quantifiedExpression.+ ^^ {
            exps => exps.reduceLeft(ast.Concattenation)
        } |
        success(ast.Epsilon)

    def quantifiedExpression =
        primaryExpression ~ quantifier.? ^^ {
            case exp ~ Some(Star) =>
                ast.KleeneStar(exp)
            case exp ~ Some(Plus) =>
                ast.Concattenation(exp, ast.KleeneStar(exp))
            case exp ~ Some(Optional) =>
                ast.Or(exp, ast.Epsilon)
            case exp ~ None =>
                exp
        }

    sealed abstract class Quantifier
    case object Star extends Quantifier
    case object Plus extends Quantifier
    case object Optional extends Quantifier

    def quantifier = "*" ^^^ Star | "+" ^^^ Plus | "?" ^^^ Optional

    def primaryExpression: Parser[ast.Expression] =
        ID ^^ {
            id => ast.RuleName(id)
        } |
        STRING_LIT ^^ {
            str => ast.StringLit(str)
        } |
        CHARACTER_CLASS ^^ {
            cc => ast.CharacterClass.parse(cc)
        } |
        "(" ~> expression <~ ")"

    def CHARACTER_CLASS = """\[(\\.|[^]])*\]""".r
    def ID = "[a-zA-Z_][a-zA-Z_0-9]*".r
    def STRING_LIT = """"[^"]*"""".r ^^ { // Fix syntax highlighting: "
        str => str.substring(1, str.length - 1)
    }

    def parse(str: String) = parseAll(grammar, str)

    def main(args: Array[String]): Unit = {
        val g = """
            grammar: X Y* | "la"+ ("li" | "lee" "e"*)? "lu" |;
            token X: "X"+ [a-zA-Z0-9_]* "X";
            token Y: "Y" [^Y]* "Y";
        """
        println("Input:")
        println(g)
        println("Result:")
        println(parse(g))
    }
}