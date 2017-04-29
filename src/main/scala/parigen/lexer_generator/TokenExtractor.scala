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

    def process(grammar: Ast.Grammar): Unit = {
        for(rule <- grammar.rules) {
            if(rule.isTokenRule) {
                addTokenType(Named(rule.name))
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
                addTokenType(Literal(str))
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
    def extractTokens(grammar: Ast.Grammar) = {
        val tokenExtractor = new TokenExtractor
        tokenExtractor.process(grammar)
        tokenExtractor.tokenIDs.toMap
    }
}