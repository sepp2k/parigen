package parigen.lexer_generator

import parigen._
import scala.collection.SortedMap
import scala.annotation.tailrec

object Automata {
    case class Nfa(alphabet: Alphabet, states: Set[State], transitions: Map[(State, Symbol), Set[State]], startingStates: Set[State], acceptingStates: Set[State])
    case class Dfa(alphabet: Alphabet, states: Set[State], transitions: Map[(State, Symbol), State], startingState: State, acceptingStates: Set[State])

    def regexesToNfa(regexes: List[(ast.Expression, Int)]): Nfa = {
        val alphabet = extractAlphabet(regexes)
        // TODO
        ???
    }

    def nfaToDfa(nfa: Nfa): Dfa = {
        // TODO
        ???
    }

    // A state doesn't have anything other than its identity. That is all we can do with a state is
    // to check whether it's equal to another state.
    // Thus we represent states using an emtpy class, which only has its object identity.
    class State

    // The symbols of our automata are numeric IDs of equivalence classes that represent all the
    // characters that are equivalent to the automata. This way, if a regex contains a character
    // range containing hundreds of characters, we don't need hundreds of transitions in our
    // automaton.
    type Symbol = Int

    type EquivalenceClass = List[(Char, Char)]

    type EquivalenceClassMap = Map[Symbol, EquivalenceClass]

    type Alphabet = SortedMap[(Char, Char), Symbol]

    // A lookup function to find a character's equivalence class could look like this:
    // def lookup(alphabet: Alphabet, char: Char): Symbol = {
    //     val ((start, end), id) = alphabet.until(char, char).max
    //     if (start <= char && char <= end) id
    //     else -1
    // }
    // However we don't need this here as we will only look up characters in the generated code.

    @tailrec
    def mergeAlphabets(alpha1: Alphabet, alpha2: Alphabet, acc: Alphabet): Alphabet = {
        if (alpha1.isEmpty) alpha2 ++ acc
        else if (alpha2.isEmpty) alpha1 ++ acc
        else {
            val ((start1, end1), id1) = alpha1.head
            val ((start2, end2), id2) = alpha2.head
            if (end1 < start2) {
                mergeAlphabets(alpha1.tail, alpha2, acc + alpha1.head)
            } else if (end2 < start1) {
                mergeAlphabets(alpha1, alpha2.tail, acc + alpha2.head)
            } else if (start1 < start2 && end1 < end2) {
                ???
            } else ???
        }
    }

    def extractAlphabet(regexes: List[(ast.Expression, Int)]): Alphabet = ???

    def regexesToDfa(regexes: List[(ast.Expression, Int)]): Dfa = {
        nfaToDfa(regexesToNfa(regexes))
    }
}