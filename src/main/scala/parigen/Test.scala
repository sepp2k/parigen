package parigen

object Test {
    def main(args: Array[String]): Unit = {
        val g = """
            grammar: X Y* | "la"+ ("li" | "lee" "e"*)? "lu" |;
            token X: "X"+ [a-zA-Z0-9_]* "X";
            token Y: "Y" [^Y]* "Y";
        """
        println("Input:")
        println(g)
        println("Result:")
        val (astOpt, diags) = Parigen.compile(g)
        diags.foreach(println)
        println(astOpt)
    }
}