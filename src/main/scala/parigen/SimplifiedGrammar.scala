package parigen

object SimplifiedGrammar {
    case class Grammar(nonTerminals: Map[String, Expression], terminals: Map[String, TokenInfo]) {
        override def toString = nonTerminals.map {case (name, value) => s"$name: $value\n"}.mkString + terminals.values.mkString("\n")
    }

    case class Expression(alternatives: Seq[Concatenation]) {
        override def toString = alternatives.mkString(" | ")
    }

    case class Concatenation(items: Seq[Item]) {
        override def toString = items.mkString(" ")
    }

    sealed abstract class Item {
        def name: String
        override def toString = name
    }

    case class NonTerminal(name: String) extends Item

    case class Terminal(name: String) extends Item

    def fromAst(g: Ast.Grammar): Grammar = new GrammarSimplifier(g).simplify
}

import SimplifiedGrammar._

private class GrammarSimplifier(g: Ast.Grammar) {
    val tokenNames = g.rules.collect { case rule if rule.isTokenRule => rule.name }.toSet
    var count = 0
    def mkId(name: String): String = {
        count += 1
        name.replaceFirst("\\$\\d+$", "") + "$" + count
    }
    def simplify: Grammar = {
        val nonTerminals = g.rules.flatMap { rule =>
            if (rule.isTokenRule) Seq()
            else {
                translateExpression(rule.exp, name = rule.name)._1
            }
        }.toMap
        val terminals = TokenExtractor.extractTokens(g).map { case (tokenType, tokenInfo) => tokenType.name -> tokenInfo }
        Grammar(nonTerminals, terminals)
    }

    def translateExpression(exp: Ast.Expression, name: String): (Map[String, Expression], Item) = exp match {
        case Ast.KleeneStar(innerExp) =>
            val (newBindings, ref) = translateExpression(innerExp, mkId(name))
            val result = Expression(Seq(Concatenation(Seq(ref, NonTerminal(name))), Concatenation(Seq())))
            (newBindings + (name -> result), NonTerminal(name))
        case _ =>
            val subExpressions = flattenAlternatives(exp).map(translateConcatentation(_, name))
            subExpressions.map(_._2) match {
                case Seq(Concatenation(Seq(item))) =>
                    (subExpressions.flatMap(_._1).toMap, item)
                case subExps =>
                    (subExpressions.flatMap(_._1).toMap + (name -> Expression(subExps)), NonTerminal(name))
            }
    }

    def flattenAlternatives(exp: Ast.Expression): Seq[Ast.Expression] = exp match {
        case Ast.Or(lhs, rhs) => flattenAlternatives(lhs) ++ flattenAlternatives(rhs)
        case _ => Seq(exp)
    }

    def translateConcatentation(exp: Ast.Expression, name: String): (Map[String, Expression], Concatenation) = {
        val items = flattenConcatenation(exp).map(translateItem(_, name))
        (items.flatMap(_._1).toMap, Concatenation(items.flatMap(_._2)))
    }

    def flattenConcatenation(exp: Ast.Expression): Seq[Ast.Expression] = exp match {
        case Ast.Concatenation(lhs, rhs) => flattenConcatenation(lhs) ++ flattenConcatenation(rhs)
        case _ => Seq(exp)
    }

    def translateItem(exp: Ast.Expression, name: String): (Map[String, Expression], Option[Item]) = exp match {
        case Ast.StringLit("") => (Map(), None)
        case Ast.StringLit(str) => (Map(), Some(Terminal(TokenInfo.Literal(str).name)))
        case Ast.RuleName(ruleName) if tokenNames.contains(ruleName) => (Map(), Some(Terminal(ruleName)))
        case Ast.RuleName(ruleName) => (Map(), Some(NonTerminal(ruleName)))
        case _ =>
            val (subExps, item) = translateExpression(exp, mkId(name))
            (subExps, Some(item))
    }
}