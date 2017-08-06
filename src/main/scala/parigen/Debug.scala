package parigen

import parigen.parser_generator.ParserGenerator
import parigen.lexer_generator.LexerGenerator
import java.nio.file.Files
import parigen.util.AutomataVisuzualizer
import java.nio.charset.StandardCharsets.UTF_8

object Debug {
    def printAst(g: Ast.Grammar) = {
        println("AST:")
        println(g)
    }

    def printSimplifiedGrammar(g: SimplifiedGrammar.Grammar) = {
        println("Simplified grammar:")
        println(g)
    }

    def printTokens(g: SimplifiedGrammar.Grammar) = {
        println("Token IDs:")
        g.terminals.values.foreach(println)
    }

    def printAlphabet(lexer: LexerGenerator.Lexer) = {
        println("Alphabet:")
        lexer.alphabet.foreach {
            case ((from, to), id) =>
                println(s"$from .. $to -> $id")
        }
    }

    def writeGraphs(lexer: LexerGenerator.Lexer) = {
        val nfaFile = Files.createTempFile("nfa", ".dot")
        Files.write(nfaFile, util.AutomataVisuzualizer.automatonToDot(lexer.nfa).getBytes(UTF_8))
        println(s"Nfa written to $nfaFile")
        val dfaFile = Files.createTempFile("dfa", ".dot")
        Files.write(dfaFile, util.AutomataVisuzualizer.automatonToDot(lexer.dfa).getBytes(UTF_8))
        println(s"Dfa written to $dfaFile")
    }

    def displayGraphs(lexer: LexerGenerator.Lexer) = {
        AutomataVisuzualizer.displayAutomaton(lexer.nfa)
        AutomataVisuzualizer.displayAutomaton(lexer.dfa)
    }

    def printFirstSets(sets: ParserGenerator.FirstSets) = {
        println("First sets:")
        sets.foreach {case (name,set) =>
            println(s"$name -> ${firstSetToString(set)}")
        }
    }

    def firstSetToString(set: ParserGenerator.FirstSet) = {
        set.map {
            case Some(sym) => sym
            case None => "ϵ"
        }
    }
}