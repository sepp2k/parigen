package parigen.parser_generator

import parigen.lexer_generator.TokenInfo
import parigen.plang.PLang
import parigen.Ast

object ParserGenerator {
    type FirstSet = Set[Option[TokenInfo.TokenID]]
    type FirstSets = Map[String, FirstSet]
    type FollowSet = Set[TokenInfo.TokenID]
    type FollowSets = Map[String, FollowSet]
    case class Parser(firstSets: FirstSets, followSets: FollowSets, parsingTable: ParsingTable, code: PLang.Module)

    def generateParser(grammar: Ast.Grammar, tokens: Map[TokenInfo.TokenType, TokenInfo]): Parser = {
        val pg = new ParserGenerator(grammar, tokens)
        Parser(pg.firstSets, null, null, null)
    }
}

import ParserGenerator._

class ParserGenerator(grammar: Ast.Grammar, tokens: Map[TokenInfo.TokenType, TokenInfo]) {
    val nonTerminals = grammar.rules.collect {
        case Ast.Rule(name, flags, body) if !flags.tokenRule =>
            name -> body
    }.toMap

    def firstSets: FirstSets = {
        nonTerminals.map {
            case (name, body) =>
                name -> firstSet(body)
        }
    }

    def firstSet(exp: Ast.Expression): FirstSet = exp match {
        case Ast.RuleName(name) =>
            tokens.get(TokenInfo.Named(name)) match {
                case Some(tokenInfo) => Set(Some(tokenInfo.id))
                case None => firstSet(nonTerminals(name))
            }
        case Ast.Concattenation(lhs, rhs) =>
            val lhsFirstSet = firstSet(lhs)
            if (lhsFirstSet.contains(None)) (lhsFirstSet - None) ++ firstSet(rhs)
            else lhsFirstSet
        case Ast.Or(lhs, rhs) =>
            firstSet(lhs) ++ firstSet(rhs)
        case Ast.StringLit("") =>
            Set(None)
        case Ast.StringLit(str) =>
            Set(Some(tokens(TokenInfo.Literal(str)).id))
        case Ast.KleeneStar(exp) =>
            firstSet(exp) + None
        case Ast.CharacterClass(_, _) =>
            sys.error("Character class in non-token rule should have been rejected by the validator.")
    }
}