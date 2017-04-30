package parigen.util

import parigen.lexer_generator.Automata

object AutomataVisuzualizer {
    def nfaToDot(nfa: Automata.Nfa): String = {
        def escape(str: String) = str.replaceAllLiterally("\\", "\\\\").replaceAllLiterally("\"", "\\\"")
        def nodeName(state: Automata.State) = {
            val name = if(nfa.initialStates.contains(state)) s"<> $state" else state.toString
            nfa.acceptingStates.get(state) match {
                case Some(id) => s"$name!!!$id"
                case None => name
            }
        }
        val trans = nfa.transitions.flatMap { case ((state, input), targets) =>
            targets.map { target =>
                val Some(((from, to), _)) = nfa.alphabet.find { case (_, id) => id == input}
                val inputString = if (from == to) s""""$input ($from)"""" else s""""$input ($from - $to)""""
                s""""${nodeName(state)}" -> "${nodeName(target)}" [type=s, label=$inputString];"""
            }
        }.mkString("\n")
        s"digraph nfa {\n$trans\n}"
    }
}