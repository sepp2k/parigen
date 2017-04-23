package parigen.lexer_generator

import parigen._
import java.io.PrintStream

object LexerGenerator {
    def generateLexer(grammar: ast.Grammar, out: PrintStream): (Map[TokenType, Int], Automata.Alphabet) = {
        val tokens = TokenExtractor.extractTokens(grammar)
        val regexes = grammar.rules.filter(_.isTokenRule).map(_.exp)
        val alphabet = Automata.extractAlphabet(regexes.toList)
        (tokens, alphabet)
    }
}