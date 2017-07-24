package parigen.lexer_generator

import parigen._
import plang.PLang
import java.io.PrintStream
import TokenInfo.TokenType

object LexerGenerator {
    case class Lexer(nfa: Nfa, dfa: Dfa, code: PLang.Module) {
        def alphabet = dfa.alphabet
    }

    def generateLexer(grammar: SimplifiedGrammar.Grammar): Lexer = {
        val tokens = grammar.terminals.values
        val nfa = Nfa.fromRegexes(tokens.map(tok => (tok.regex, tok.id)).toList)
        val dfa = Dfa.fromNfa(nfa)
        val tokenLookup = tokens.map { tokenInfo =>
            tokenInfo.id -> tokenInfo.tokenType
        }.toMap
        val code = PLangGenerator.fromDfa(dfa, tokenLookup)
        Lexer(nfa, dfa, code)
    }
}