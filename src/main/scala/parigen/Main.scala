package parigen

import scala.io.Source
import sexyopt.SexyOpt

object Main extends SexyOpt {
    override val programName = "parigen"
    override val programDescription = "Generate a lexer, a parser and IDE support from a grammar."

    val writeGraphs = flag("write-graphs", "Write the graphs of the generated automata to temporary dot files for debugging purposes.")
    val displayGraphs = flag("display-graphs", "Display the graphs of the generated automata directly using `dot -Tx11` (requires GraphViz and X11).")
    val printAst = flag("print-ast", "Print the AST of the input grammar to the screen for debugging purposes")
    val printAlphabet = flag("print-alphabet", "Print the generated alphabet mapping to the screen for debugging purposes")
    val printTokens = flag("print-tokens", "Print the Tokens' names and IDs to the screen for debugging purposes")
    val outDir = option("out-dir", 'o', "The directory in which the generated source files will be placed (default: the current directory)", ".")
    val target = option("target", 't', "The target language for code generation (default: scala; available: typescript, scala)", "scala")
    val packageName = option("package", 'p', "The package to generate code into")
    val filename = optionalPosArg("filename", "The name of the file containing the parigen grammar.", "src/main/parigen/parigen.parigen")

    def main(args: Array[String]): Unit = {
        parse(args)
        val source = Source.fromFile(filename)
        val g = try source.mkString finally source.close()
        val debug = Parigen.DebugOptions(writeGraphs = writeGraphs, displayGraphs = displayGraphs, printAst = printAst,
                                         printAlphabet = printAlphabet, printTokens = printTokens)
        val diags = Parigen.compile(g, outDir, debug)
        diags.foreach(System.err.println)
    }
}