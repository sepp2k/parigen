package parigen.lexer_generator

import parigen._
import scala.collection.SortedSet
import scala.annotation.tailrec

object Automata {
    case class Nfa(alphabet: Alphabet, states: Set[State], transitions: Map[(State, Symbol), Set[State]], startingStates: Set[State], acceptingStates: Set[State])
    case class Dfa(alphabet: Alphabet, states: Set[State], transitions: Map[(State, Symbol), State], startingState: State, acceptingStates: Set[State])

    def regexesToNfa(regexes: List[(ast.Expression, Int)]): Nfa = {
        val alphabet = extractAlphabet(regexes.map(_._1))
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

    // An equivalence class is a pair of characters, representing an inclusive range of chatacters
    // that are equivalent for the purposes of the automaton. That is if, for example, all characters
    // between a and z would lead to the same transitions in all cases, ('a','z') would constitute an
    // equivalence class.
    // This way, if a regex contains a character range containing hundreds of characters, we don't
    // need hundreds of transitions in our automaton.
    type EquivalenceClass = (Char, Char)

    type Alphabet = SortedSet[EquivalenceClass]
    val emptyAlphabet: Alphabet = SortedSet[EquivalenceClass]()

    // A lookup function to find a character's equivalence class could look like this:
    // def lookup(alphabet: Alphabet, char: Char): Symbol = {
    //     val ((start, end), id) = alphabet.until(char, char).max
    //     if (start <= char && char <= end) id
    //     else -1
    // }
    // However we don't need this here as we will only look up characters in the generated code.

    private def pred(c: Char): Char = (c - 1).toChar
    private def succ(c: Char): Char = (c + 1).toChar

    @tailrec
    def mergeAlphabets(alpha1: Alphabet, alpha2: Alphabet, acc: Alphabet): Alphabet = {
        if (alpha1.isEmpty) alpha2 ++ acc
        else if (alpha2.isEmpty) alpha1 ++ acc
        else {
            val (start1, end1) = alpha1.head
            val (start2, end2) = alpha2.head
            if (start1 == start2) {
                if (end1 == end2) {
                    mergeAlphabets(alpha1.tail, alpha2.tail, acc + (start1 -> end1))
                } else if (end1 < end2) {
                    mergeAlphabets(alpha1.tail, alpha2.tail + (succ(end1) -> end2), acc + (start1 -> end1))
                } else {
                    mergeAlphabets(alpha1.tail + (succ(end2) -> end1), alpha2.tail, acc + (start2 -> end2))
                }
            } else if (end1 == end2) {
                if(start1 < start2) {
                    mergeAlphabets(alpha1.tail, alpha2, acc + (start1 -> pred(start2)))
                } else {
                    mergeAlphabets(alpha1, alpha2.tail, acc + (start2 -> pred(start1)))
                }
            } else if (end1 < start2) {
                mergeAlphabets(alpha1.tail, alpha2, acc + (start1 -> end1))
            } else if (end2 < start1) {
                mergeAlphabets(alpha1, alpha2.tail, acc + (start2 -> end2))
            } else if (start1 < start2 && end1 < end2) {
                mergeAlphabets( alpha1.tail + (start2 -> end1), alpha2.tail + (succ(end1) -> end2), acc + (start1 -> pred(start2)))
            } else if (start2 < start1 && end2 < end1) {
                mergeAlphabets( alpha1.tail + (start1 -> end2), alpha2.tail + (succ(end2) -> end1), acc + (start2 -> pred(start1)))
            } else if (start1 < start2 && end2 < end1) {
                mergeAlphabets( alpha1.tail + (succ(end2) -> end1), alpha2.tail + (start2 -> end2), acc + (start1 -> pred(start2)))
            } else {
                assert(start2 < start1 && end1 < end2)
                mergeAlphabets( alpha1.tail + (start1 -> end1), alpha2.tail + (succ(end1) -> end2), acc + (start2 -> pred(start1)))
            }
        }
    }

    def invert(alpha: Alphabet) = {
        alpha.zip(alpha.tail).map {
            case ((_, end1), (start2, _)) => succ(end1) -> pred(start2)
        } + (Char.MinValue -> pred(alpha.min._1)) + (succ(alpha.max._2) -> Char.MaxValue)
    }

    def extractAlphabet(regex: ast.Expression): Alphabet = regex match {
        case ast.Epsilon =>
            emptyAlphabet
        case ast.StringLit(str) =>
            SortedSet(str.map(c => (c,c)) : _*)
        case ast.CharacterClass(ranges, false) =>
            SortedSet(ranges : _*)
        case ast.CharacterClass(ranges, true) =>
            invert(SortedSet(ranges : _*))
        case ast.KleeneStar(arg) =>
            extractAlphabet(arg)
        case ast.Or(lhs, rhs) =>
            mergeAlphabets(extractAlphabet(lhs), extractAlphabet(rhs), emptyAlphabet)
        case ast.Concattenation(lhs, rhs) =>
            mergeAlphabets(extractAlphabet(lhs), extractAlphabet(rhs), emptyAlphabet)
        case ast.RuleName(_) =>
            sys.error("Rule name in regex should have been rejected.")
    }

    def extractAlphabet(regexes: List[ast.Expression]): Alphabet = {
        regexes.map(extractAlphabet).foldLeft(emptyAlphabet) {
            (acc, alpha) => mergeAlphabets(acc, alpha, emptyAlphabet)
        }
    }

    def regexesToDfa(regexes: List[(ast.Expression, Int)]): Dfa = {
        nfaToDfa(regexesToNfa(regexes))
    }
}