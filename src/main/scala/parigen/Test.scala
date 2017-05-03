package parigen

import scala.io.Source

object Test {
    def main(args: Array[String]): Unit = {
        val filename =
            if (args.length > 0) args(1)
            else "src/main/parigen/parigen.pig"
        val source = Source.fromFile(filename)
        val g = try source.mkString finally source.close()
        println("Input:")
        println(g)
        val diags = Parigen.compile(g, System.out, printStages = true)
        diags.foreach(System.err.println)
    }
}