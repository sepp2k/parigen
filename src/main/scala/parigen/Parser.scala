package parigen

import scala.util.parsing.combinator._

object Parser extends RegexParsers {
    def grammar = rule.* ^^ {
        rules => ast.Grammar(rules.toMap)
    }

    def rule = (ID <~ ":") ~ expression <~ ";" ^^ {
        case name ~ exp => name -> exp
    }

    def expression: Parser[ast.Expression] =
        rep1sep(sequence, "|") ^^ {
            exps => exps.reduce(ast.Or)
        }

    def sequence =
        quantifiedExpression.+ ^^ {
            exps => exps.reduceLeft(ast.Seq)
        } |
        success(ast.Epsilon)

    def quantifiedExpression =
        primaryExpression ~ quantifier.? ^^ {
            case exp ~ Some(Star) =>
                ast.KleeneStar(exp)
            case exp ~ Some(Plus) =>
                ast.Seq(exp, ast.KleeneStar(exp))
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

    def primaryExpression =
        ID ^^ {
            id => ast.RuleName(id)
        } |
        STRING_LIT ^^ {
            str => ast.StringLit(str)
        } |
        "(" ~> expression <~ ")"

    def ID = "[a-zA-Z_][a-zA-Z_0-9]*".r
    def STRING_LIT = "\"[^\"]*\"".r ^^ {
        str => str.substring(1, str.length - 1)
    }

    def parse(str: String) = parseAll(grammar, str)

    def main(args: Array[String]): Unit = {
        val g = """
            grammar: x y* | "la"+ ("li" | "lee" "e"*)? "lu" |;
            x: "X";
            y: "Y";
        """
        println("Input:")
        println(g)
        println("Result:")
        println(parse(g))
    }
}