package parigen

import scala.util.parsing.combinator._

object Parser extends RegexParsers {
    def grammar = rule.* ^^ {
        rules => Ast.Grammar(rules)
    }

    def rule =
        "token".? ~ (ID <~ ":") ~ expression <~ ";" ^^ {
            case tokenOpt ~ name ~ exp =>
                Ast.Rule(name, Ast.RuleFlags(tokenRule = tokenOpt.isDefined), exp)
        }

    def expression: Parser[Ast.Expression] =
        rep1sep(sequence, "|") ^^ {
            exps => exps.reduce(Ast.Or)
        }

    def sequence =
        quantifiedExpression.+ ^^ {
            exps => exps.reduceLeft(Ast.Concatenation)
        } |
        success(Ast.epsilon)

    def quantifiedExpression =
        primaryExpression ~ quantifier.? ^^ {
            case exp ~ Some(Star) =>
                Ast.KleeneStar(exp)
            case exp ~ Some(Plus) =>
                Ast.Concatenation(exp, Ast.KleeneStar(exp))
            case exp ~ Some(Optional) =>
                Ast.Or(exp, Ast.epsilon)
            case exp ~ None =>
                exp
        }

    sealed abstract class Quantifier
    case object Star extends Quantifier
    case object Plus extends Quantifier
    case object Optional extends Quantifier

    def quantifier = "*" ^^^ Star | "+" ^^^ Plus | "?" ^^^ Optional

    def primaryExpression: Parser[Ast.Expression] =
        ID ^^ {
            id => Ast.RuleName(id)
        } |
        STRING_LIT ^^ {
            str => Ast.StringLit.parse(str)
        } |
        CHARACTER_CLASS ^^ {
            cc => Ast.CharacterClass.parse(cc)
        } |
        "." ^^^ Ast.CharacterClass(Seq(), negated = true) |
        "(" ~> expression <~ ")"

    def CHARACTER_CLASS = """\[(\\.|[^]\\])*\]""".r
    def ID = "[a-zA-Z_][a-zA-Z_0-9]*".r
    def STRING_LIT = """"(\\.|[^"\\])*"""".r

    def parse(str: String) = parseAll(grammar, str)
}