package parigen

import lexer_generator.TokenInfo

object SimplifiedGrammar {
    case class Grammar(nonTerminals: Map[String, Expression], terminals: Map[String, TokenInfo]) {
        override def toString = nonTerminals.map {case (name, value) => s"$name: $value\n"}.mkString + terminals.values.mkString("\n")
    }

    case class Expression(alternatives: Seq[Concatenation])

    case class Concatenation(items: Seq[Item])

    sealed abstract class Item
    case class NonTerminal(name: String) extends Item
    case class Terminal(name: String) extends Item

    def fromAst(g: Ast.Grammar): Grammar = ???
}