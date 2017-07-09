package parigen.util

import scala.sys.process.Process
import java.io.ByteArrayInputStream
import parigen.lexer_generator.Automaton

object AutomataVisuzualizer {
    def automatonToDot(automaton: Automaton): String = {
        def escape(chr: Char) =
            if (chr == '"') "\\\""
            else if (chr == '\\') "\\\\"
            else if (Character.isISOControl(chr)) s"\\\\${chr.toInt}"
            else chr.toString
        def nodeName(state: Automaton.State) = {
            val name = if(automaton.isInitialState(state)) s"<> $state" else state.toString
            automaton.isAccepting(state) match {
                case Some(id) => s"$name!!!$id"
                case None => name
            }
        }
        val trans = automaton.flatTransitions.map { case (state, input, target) =>
            val Some(((from, to), _)) = automaton.alphabet.find { case (_, id) => id == input}
            val inputString = if (from == to) s""""$input (${escape(from)})"""" else s""""$input (${escape(from)} - ${escape(to)})""""
            s""""${nodeName(state)}" -> "${nodeName(target)}" [type=s, label=$inputString];"""
        }.mkString("\n")
        s"digraph ${automaton.name} {\nlabel = ${automaton.name};\n$trans\n}"
    }

    def displayAutomaton(automaton: Automaton): Unit = {
        (Process(Seq("dot", "-Tx11", "-Ksfdp")) #< new ByteArrayInputStream(automatonToDot(automaton).getBytes)).run
    }
}