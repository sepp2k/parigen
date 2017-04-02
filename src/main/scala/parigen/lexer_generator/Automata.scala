package parigen.lexer_generator

import parigen._
import scala.collection.SortedSet
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

    class State
    type Symbol = (Char, Char)
    type Alphabet = SortedSet[Symbol]

    @tailrec
    def mergeAlphabets(alpha1: Alphabet, alpha2: Alphabet, acc: Alphabet): Alphabet = {
        if (alpha1.isEmpty) alpha2 ++ acc
        else if (alpha2.isEmpty) alpha1 ++ acc
        else {
            // TODO
            mergeAlphabets(???, ???, ???)
        }
    }

    def extractAlphabet(regexes: List[(ast.Expression, Int)]): Alphabet = ???

    def regexesToDfa(regexes: List[(ast.Expression, Int)]): Dfa = {
        nfaToDfa(regexesToNfa(regexes))
    }
}