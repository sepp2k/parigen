package parigen

import scala.io.Source
import sexyopt.SexyOpt

object Main extends SexyOpt {
    override val programName = "parigen"
    override val programDescription = "Generate a lexer, a parser and IDE support from a grammar."

    val writeGraphs = flag("write-graphs", "Write the graphs of the generated automata to temporary dot files for debugging purposes")
    val displayGraphs = flag("display-graphs", "Display the graphs of the generated automata directly using `dot -Tx11` (requires GraphViz and X11)")
    val printAst = flag("print-ast", "Print the AST of the input grammar to the screen for debugging purposes")
    val printSimplifiedGrammar = flag("print-simplified-grammar", "Prints the simplified grammar to the screen for debugging purposes")
    val printAlphabet = flag("print-alphabet", "Print the generated alphabet mapping to the screen for debugging purposes")
    val printTokens = flag("print-tokens", "Print the Tokens' names and IDs to the screen for debugging purposes")
    val printFirstAndFollow = flag("print-first-and-follow", "Print the first and follow sets of the grammar's non-terminals to the screen for debugging purposes")
    val outDir = option("out-dir", 'o', "The directory in which the generated source files will be placed (default: the current directory)", ".")
    val target = option("target", 't', "The target language for code generation (default: typescript; available: typescript, scala)", "typescript")
    val packageName = option("package", 'p', "The package to generate code into")
    val filename = optionalPosArg("filename", "The name of the file containing the parigen grammar", "src/main/parigen/parigen.parigen")

    def main(args: Array[String]): Unit = {
        parse(args)
        val language = target.value match {
            case "scala" => Parigen.Scala
            case "typescript" => Parigen.TypeScript
            case _ =>
                System.err.println(s"Unsupported language: $target")
                sys.exit(1)
        }
        val source = Source.fromFile(filename)
        val g = try source.mkString finally source.close()
        val debug = Parigen.DebugOptions(writeGraphs = writeGraphs, displayGraphs = displayGraphs, printAst = printAst,
                                         printSimplifiedGrammar = printSimplifiedGrammar, printAlphabet = printAlphabet,
                                         printTokens = printTokens, printFirstAndFollow = printFirstAndFollow)
        val result = Parigen.compile(g, outDir, language, packageName, debug)
        result.diagnostics.foreach(System.err.println)
        result match {
            case _ : Parigen.CompilationError =>
                System.err.println("Compilation failed")
                sys.exit(1)
            case result : Parigen.CompilationSuccess =>
                println(s"Lexer written to ${result.lexerFile}")
        }
    }
}