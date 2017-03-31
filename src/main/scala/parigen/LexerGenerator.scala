package parigen

import scala.collection.mutable
import java.io.PrintStream

class LexerGenerator(out: PrintStream) {
    import LexerGenerator._

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
        // TODO: Generate code
    }

    def findStringLits(exp: ast.Expression) {
        // TODO: implement
    }
}

object LexerGenerator {
    sealed abstract class TokenType
    case class Named(name: String) extends TokenType
    case class Literal(value: String) extends TokenType

    def generateLexer(grammar: ast.Grammar, out: PrintStream): Map[TokenType, Int] = {
        val lexerGen = new LexerGenerator(out)
        lexerGen.process(grammar)
        lexerGen.tokenIDs.toMap
    }
}