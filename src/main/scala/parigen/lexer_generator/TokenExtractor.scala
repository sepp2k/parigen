package parigen.lexer_generator

import parigen._
import scala.collection.mutable
import TokenInfo._

class TokenExtractor {
    val tokenIDs = new mutable.HashMap[TokenType, TokenInfo]
    var counter: TokenID = 0

    private def addTokenType(tt: TokenType, regex: Ast.Expression): Unit = {
        if (!tokenIDs.isDefinedAt(tt)) {
            tokenIDs(tt) = TokenInfo(tokenType = tt, regex = regex, id = counter)
            counter += 1
        }
    }

    def process(grammar: Ast.Grammar): Unit = {
        for(rule <- grammar.rules) {
            if(rule.isTokenRule) {
                addTokenType(Named(rule.name), rule.exp)
            } else {
                findStringLits(rule.exp)
            }
        }
    }

    def findStringLits(exp: Ast.Expression) {
        exp match {
            case Ast.CharacterClass(_, _) | Ast.RuleName(_) | Ast.StringLit("") =>
                // do nothing
            case Ast.StringLit(str) =>
                addTokenType(Literal(str), exp)
            case Ast.Or(lhs, rhs) =>
                findStringLits(lhs)
                findStringLits(rhs)
            case Ast.Concattenation(lhs, rhs) =>
                findStringLits(lhs)
                findStringLits(rhs)
            case Ast.KleeneStar(arg) =>
                findStringLits(arg)
        }
    }
}

object TokenExtractor {
    def extractTokens(grammar: Ast.Grammar): Map[TokenType, TokenInfo] = {
        val tokenExtractor = new TokenExtractor
        tokenExtractor.process(grammar)
        tokenExtractor.tokenIDs.toMap
    }
}