package parigen

import java.io.PrintStream
import lexer_generator.LexerGenerator

object Parigen {
    def compile(src: String, out: PrintStream, printStages: Boolean = false) = {
        Parser.parse(src) match {
            case Parser.Success(ast, _) =>
                if (printStages) {
                    System.err.println("AST:")
                    System.err.println(ast)
                }
                val diags = Validator.validate(ast)
                if (diags.exists(_.severity == Validator.Error)) diags
                else {
                    val (tokenIDs, nfa) = LexerGenerator.generateLexer(ast, out)
                    if(printStages) {
                        println("Token IDs:")
                        tokenIDs.foreach(println)
                        println("Alphabet:")
                        nfa.alphabet.foreach {
                            case ((from, to), id) => println(s"$from .. $to -> $id")
                        }
                        println("Nfa:")
                        println(nfa)
                    }
                    diags
                }
            case Parser.NoSuccess(message, _) =>
                List( Validator.Diagnostic( Validator.Error, None, s"Illegal syntax: $message" ))
        }
    }
}