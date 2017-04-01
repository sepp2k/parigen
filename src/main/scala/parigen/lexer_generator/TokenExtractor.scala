package parigen.lexer_generator

import parigen._
import scala.collection.mutable

class TokenExtractor {
    val tokenIDs = new mutable.HashMap[TokenType, Int]
    var counter = 0

    private def addTokenType(tt: TokenType): Unit = {
        if (!tokenIDs.isDefinedAt(tt)) {
            tokenIDs(tt) = counter
            counter += 1
        }
    }

    def process(grammar: ast.Grammar): Unit = {
        for(rule <- grammar.rules) {
            if(rule.isTokenRule) {
                addTokenType(Named(rule.name))
            } else {
                findStringLits(rule.exp)
            }
        }
    }

    def findStringLits(exp: ast.Expression) {
        exp match {
            case ast.StringLit(str) =>
                addTokenType(Literal(str))
            case ast.CharacterClass(_, _) | ast.Epsilon | ast.RuleName(_) =>
                // do nothing
            case ast.Or(lhs, rhs) =>
                findStringLits(lhs)
                findStringLits(rhs)
            case ast.Concattenation(lhs, rhs) =>
                findStringLits(lhs)
                findStringLits(rhs)
            case ast.KleeneStar(arg) =>
                findStringLits(arg)
        }
    }
}

object TokenExtractor {
    def extractTokens(grammar: ast.Grammar) = {
        val tokenExtractor = new TokenExtractor
        tokenExtractor.process(grammar)
        tokenExtractor.tokenIDs.toMap
    }
}