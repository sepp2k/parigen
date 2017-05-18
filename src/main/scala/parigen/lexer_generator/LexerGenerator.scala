package parigen.lexer_generator

import parigen._
import java.io.PrintStream
import TokenInfo.TokenType

object LexerGenerator {
    def generateLexer(grammar: Ast.Grammar): (Map[TokenType, TokenInfo], Nfa) = {
        val tokens = TokenExtractor.extractTokens(grammar)
        val nfa = Nfa.fromRegexes(tokens.values.map(tok => (tok.regex, tok.id)).toList)
        (tokens, nfa)
    }
}