package parigen.lexer_generator

import parigen._
import java.io.PrintStream

object LexerGenerator {
    def generateLexer(grammar: ast.Grammar, out: PrintStream): Map[TokenType, Int] = {
        TokenExtractor.extractTokens(grammar)
    }
}