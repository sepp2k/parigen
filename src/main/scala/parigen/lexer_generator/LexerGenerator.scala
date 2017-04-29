package parigen.lexer_generator

import parigen._
import java.io.PrintStream
import TokenExtractor.TokenID

object LexerGenerator {
    def generateLexer(grammar: Ast.Grammar, out: PrintStream): (Map[TokenType, TokenID], Automata.Alphabet) = {
        val tokens = TokenExtractor.extractTokens(grammar)
        val regexes = grammar.rules.filter(_.isTokenRule).map(_.exp)
        val alphabet = Automata.extractAlphabet(regexes.toList)
        (tokens, alphabet)
    }
}