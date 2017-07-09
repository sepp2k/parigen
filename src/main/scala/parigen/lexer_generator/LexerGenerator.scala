package parigen.lexer_generator

import parigen._
import java.io.PrintStream
import TokenInfo.TokenType

object LexerGenerator {
    case class Lexer(tokens: Map[TokenType, TokenInfo], nfa: Nfa, dfa: Dfa) {
        def alphabet = dfa.alphabet
    }

    def generateLexer(grammar: Ast.Grammar): Lexer = {
        val tokens = TokenExtractor.extractTokens(grammar)
        val nfa = Nfa.fromRegexes(tokens.values.map(tok => (tok.regex, tok.id)).toList)
        val dfa = Dfa.fromNfa(nfa)
        Lexer(tokens, nfa, dfa)
    }
}