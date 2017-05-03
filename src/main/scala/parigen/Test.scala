package parigen

import scala.io.Source

object Test {
    def main(args: Array[String]): Unit = {
        val defaultFile = "src/main/parigen/parigen.pig"
        val (filename, debugMode) = args match {
            case Array() =>
                (defaultFile, Parigen.DebugMode.GraphsToFiles)
            case Array("--display-graphs") =>
                (defaultFile, Parigen.DebugMode.DisplayGraphs)
            case Array(filename) =>
                (filename, Parigen.DebugMode.GraphsToFiles)
            case Array("--display-graphs", filename) =>
                (filename, Parigen.DebugMode.DisplayGraphs)
        }
        val source = Source.fromFile(filename)
        val g = try source.mkString finally source.close()
        println("Input:")
        println(g)
        val diags = Parigen.compile(g, debugMode)
        diags.foreach(System.err.println)
    }
}