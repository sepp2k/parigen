package parigen

import java.io.PrintStream
import lexer_generator.LexerGenerator
import parser_generator.ParserGenerator

object Parigen {
    case class DebugOptions(writeGraphs: Boolean, displayGraphs: Boolean, printAst: Boolean, printSimplifiedGrammar: Boolean,
                            printAlphabet: Boolean, printTokens: Boolean, printFirstAndFollow: Boolean)
    sealed abstract class Language
    case object Scala extends Language
    case object TypeScript extends Language

    sealed abstract class CompilationResult {
        def diagnostics: Seq[Validator.Diagnostic]
    }

    case class CompilationSuccess(
        diagnostics: Seq[Validator.Diagnostic],
        lexer: LexerGenerator.Lexer, lexerFile: String,
        parser: ParserGenerator.Parser
    ) extends CompilationResult

    case class CompilationError(diagnostics: Seq[Validator.Diagnostic]) extends CompilationResult

    def compile(src: String, outDir: String, language: Language, packageName: Option[String], debug: DebugOptions): CompilationResult = {
        ParigenParser.parse(src) match {
            case ParigenParser.Success(ast, _) =>
                if (debug.printAst) Debug.printAst(ast)
                val diags = Validator.validate(ast)
                if (diags.exists(_.severity == Validator.Error)) CompilationError(diags)
                else {
                    val simplifiedGrammar = SimplifiedGrammar.fromAst(ast)
                    if (debug.printSimplifiedGrammar) Debug.printSimplifiedGrammar(simplifiedGrammar)
                    if (debug.printTokens) Debug.printTokens(simplifiedGrammar)
                    val lexer = LexerGenerator.generateLexer(simplifiedGrammar)
                    if (debug.printAlphabet) Debug.printAlphabet(lexer)
                    if (debug.writeGraphs) Debug.writeGraphs(lexer)
                    if (debug.displayGraphs) Debug.displayGraphs(lexer)
                    val lexerFile = language match {
                        case Scala =>
                            val outFile = new PrintStream(s"$outDir/Lexer.scala")
                            new plang.ScalaGenerator(outFile, 4).generate(lexer.code, packageName)
                            outFile.close
                            s"$outDir/Lexer.scala"
                        case TypeScript =>
                            val outFile = new PrintStream(s"$outDir/lexer.ts")
                            new plang.TypeScriptGenerator(outFile, 4).generate(lexer.code)
                            outFile.close
                            s"$outDir/lexer.ts"
                    }
                    val parser = ParserGenerator.generateParser(simplifiedGrammar)
                    if (debug.printFirstAndFollow) Debug.printFirstAndFollow(parser)
                    CompilationSuccess(diags, lexer, lexerFile, parser)
                }
            case ParigenParser.NoSuccess(message, rest) =>
                CompilationError(Seq(Validator.Diagnostic(Validator.Error, None, s"Illegal syntax at ${rest.pos}: $message")))
        }
    }
}