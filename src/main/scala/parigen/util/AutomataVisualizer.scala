package parigen.util

import scala.sys.process.Process
import java.io.ByteArrayInputStream
import parigen.lexer_generator.Automata

object AutomataVisuzualizer {
    def nfaToDot(nfa: Automata.Nfa): String = {
        def escape(chr: Char) =
            if (chr == '"') "\\\""
            else if (chr == '\\') "\\\\"
            else if (Character.isISOControl(chr)) s"\\\\${chr.toInt}"
            else chr.toString
        def nodeName(state: Automata.State) = {
            val name = if(nfa.initialStates.contains(state)) s"<> $state" else state.toString
            nfa.acceptingStates.get(state) match {
                case Some(id) => s"$name!!!$id"
                case None => name
            }
        }
        val trans = nfa.flatTransitions.map { case (state, input, target) =>
            val Some(((from, to), _)) = nfa.alphabet.find { case (_, id) => id == input}
            val inputString = if (from == to) s""""$input (${escape(from)})"""" else s""""$input (${escape(from)} - ${escape(to)})""""
            s""""${nodeName(state)}" -> "${nodeName(target)}" [type=s, label=$inputString];"""
        }.mkString("\n")
        s"digraph nfa {\n$trans\n}"
    }

    def displayNfa(nfa: Automata.Nfa): Unit = {
        (Process(Seq("dot", "-Tx11", "-Ksfdp")) #< new ByteArrayInputStream(nfaToDot(nfa).getBytes)).!
    }
}