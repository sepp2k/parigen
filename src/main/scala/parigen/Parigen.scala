package parigen

import java.nio.file.Files
import java.nio.charset.StandardCharsets.UTF_8
import lexer_generator.LexerGenerator

object Parigen {
    sealed abstract class DebugMode
    object DebugMode {
        case object None extends DebugMode
        case object GraphsToFiles extends DebugMode
        case object DisplayGraphs extends DebugMode
    }

    def compile(src: String, debugMode: DebugMode = DebugMode.None) = {
        val printStages = debugMode != DebugMode.None
        Parser.parse(src) match {
            case Parser.Success(ast, _) =>
                if (printStages) {
                    System.err.println("AST:")
                    System.err.println(ast)
                }
                val diags = Validator.validate(ast)
                if (diags.exists(_.severity == Validator.Error)) diags
                else {
                    val lexer = LexerGenerator.generateLexer(ast)
                    if(printStages) {
                        println("Token IDs:")
                        lexer.tokens.foreach(println)
                        println("Alphabet:")
                        lexer.alphabet.foreach {
                            case ((from, to), id) => println(s"$from .. $to -> $id")
                        }

                        if (debugMode == DebugMode.GraphsToFiles) {
                            val nfaFile = Files.createTempFile("nfa", ".dot")
                            Files.write(nfaFile, util.AutomataVisuzualizer.automatonToDot(lexer.nfa).getBytes(UTF_8))
                            println(s"Nfa written to $nfaFile")
                            val dfaFile = Files.createTempFile("dfa", ".dot")
                            Files.write(dfaFile, util.AutomataVisuzualizer.automatonToDot(lexer.dfa).getBytes(UTF_8))
                            println(s"Dfa written to $dfaFile")
                        } else {
                            util.AutomataVisuzualizer.displayAutomaton(lexer.nfa)
                            util.AutomataVisuzualizer.displayAutomaton(lexer.dfa)
                        }
                    }
                    diags
                }
            case Parser.NoSuccess(message, rest) =>
                List( Validator.Diagnostic( Validator.Error, None, s"Illegal syntax at ${rest.pos}: $message" ))
        }
    }
}