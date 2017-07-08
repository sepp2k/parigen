package parigen

import scala.io.Source
import sexyopt.SexyOpt

object Test extends SexyOpt {
    override val programName = "parigen"
    override val programDescription = "Generate a lexer, a parser and IDE support from a grammar."

    val displayGraphs = flag("display-graphs", "Display the graphs of the generated automata directly using `dot -Tx11` instead of generating dot files.")
    val filename = optionalPosArg("filename", "The name of the file containing the parigen grammar.", "src/main/parigen/parigen.pig")

    def main(args: Array[String]): Unit = {
        parse(args)
        val debugMode = if (displayGraphs) Parigen.DebugMode.DisplayGraphs else Parigen.DebugMode.GraphsToFiles
        val source = Source.fromFile(filename)
        val g = try source.mkString finally source.close()
        println("Input:")
        println(g)
        val diags = Parigen.compile(g, debugMode)
        diags.foreach(System.err.println)
    }
}