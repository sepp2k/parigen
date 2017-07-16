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
        val tokenLookup = tokens.map {
            case (_, tokenInfo) => tokenInfo.id -> tokenInfo.tokenType
        }
        val code = PLangGenerator.fromDfa(dfa, tokenLookup)
        Lexer(tokens, nfa, dfa, code)
    }
}