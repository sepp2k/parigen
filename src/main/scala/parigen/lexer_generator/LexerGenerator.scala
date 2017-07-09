package parigen.lexer_generator

import parigen._
import plang.PLang
import java.io.PrintStream
import TokenInfo.TokenType

object LexerGenerator {
    case class Lexer(tokens: Map[TokenType, TokenInfo], nfa: Nfa, dfa: Dfa, code: PLang.Module) {
        def alphabet = dfa.alphabet
    }

    def generateLexer(grammar: Ast.Grammar): Lexer = {
        val tokens = TokenExtractor.extractTokens(grammar)
        val nfa = Nfa.fromRegexes(tokens.values.map(tok => (tok.regex, tok.id)).toList)
        val dfa = Dfa.fromNfa(nfa)
        val code = CodeGenerator.fromDfa(dfa)
        Lexer(tokens, nfa, dfa, code)
    }
}