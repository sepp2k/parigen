package parigen

object Test {
    def main(args: Array[String]): Unit = {
        val g = """
            grammar: X Y* | "la"+ ("li" | "lee" "e"*)? "lu" |;
            token X: "X"+ [a-zA-Z0-9_]* "X";
            token Y: "Y" [^Y]* "Y";
        """
        System.err.println("Input:")
        System.err.println(g)
        val diags = Parigen.compile(g, System.out, printStages = true)
        diags.foreach(System.err.println)
    }
}