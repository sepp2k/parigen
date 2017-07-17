package parigen

import java.nio.file.Files
import java.nio.charset.StandardCharsets.UTF_8
import java.io.PrintStream
import lexer_generator.LexerGenerator

object Parigen {
    case class DebugOptions(writeGraphs: Boolean, displayGraphs: Boolean, printAst: Boolean, printAlphabet: Boolean, printTokens: Boolean)
    sealed abstract class Language
    case object Scala extends Language
    case object TypeScript extends Language

    def compile(src: String, outDir: String, language: Language, packageName: Option[String], debug: DebugOptions) = {
        Parser.parse(src) match {
            case Parser.Success(ast, _) =>
                if (debug.printAst) {
                    System.err.println("AST:")
                    System.err.println(ast)
                }
                val diags = Validator.validate(ast)
                if (diags.exists(_.severity == Validator.Error)) diags
                else {
                    val lexer = LexerGenerator.generateLexer(ast)
                    if (debug.printTokens) {
                        println("Token IDs:")
                        lexer.tokens.foreach(println)
                    }
                    if (debug.printAlphabet) {
                        println("Alphabet:")
                        lexer.alphabet.foreach {
                            case ((from, to), id) => println(s"$from .. $to -> $id")
                        }
                    }
                    if (debug.writeGraphs) {
                        val nfaFile = Files.createTempFile("nfa", ".dot")
                        Files.write(nfaFile, util.AutomataVisuzualizer.automatonToDot(lexer.nfa).getBytes(UTF_8))
                        println(s"Nfa written to $nfaFile")
                        val dfaFile = Files.createTempFile("dfa", ".dot")
                        Files.write(dfaFile, util.AutomataVisuzualizer.automatonToDot(lexer.dfa).getBytes(UTF_8))
                        println(s"Dfa written to $dfaFile")
                    }
                    if (debug.displayGraphs) {
                        util.AutomataVisuzualizer.displayAutomaton(lexer.nfa)
                        util.AutomataVisuzualizer.displayAutomaton(lexer.dfa)
                    }
                    language match {
                        case Scala =>
                            val outFile = new PrintStream(s"$outDir/Lexer.scala")
                            new plang.ScalaGenerator(outFile, 4).generate(lexer.code, packageName)
                            outFile.close
                            println(s"Lexer written to $outDir/Lexer.scala")
                            diags
                        case TypeScript =>
                            val outFile = new PrintStream(s"$outDir/lexer.ts")
                            new plang.TypeScriptGenerator(outFile, 4).generate(lexer.code)
                            outFile.close
                            println(s"Lexer written to $outDir/lexer.ts")
                            diags
                    }
                }
            case Parser.NoSuccess(message, rest) =>
                List( Validator.Diagnostic( Validator.Error, None, s"Illegal syntax at ${rest.pos}: $message" ))
        }
    }
}