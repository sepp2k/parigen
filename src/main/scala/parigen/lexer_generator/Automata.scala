package parigen.lexer_generator

import parigen._
import scala.collection.{SortedSet, SortedMap}
import scala.annotation.tailrec
import TokenInfo.TokenID

object Automata {
    type Symbol = Int
    case class Nfa(
        alphabet: IndexedAlphabet,
        states: Set[State],
        transitions: Map[(State, Symbol), Set[State]],
        initialStates: Set[State],
        acceptingStates: Map[State, TokenID]
    ) {
        def or(other: Nfa) = {
            require(alphabet == other.alphabet)
            Nfa(alphabet, states ++ other.states, transitions ++ other.transitions,
                initialStates ++ other.initialStates, acceptingStates ++ other.acceptingStates)
        }

        def concat(other: Nfa) = {
            require(alphabet == other.alphabet)
            val trans = transitions.map {
                case ((state, sym), targets) =>
                    if (targets.exists(t => acceptingStates.isDefinedAt(t))) (state, sym) -> (targets ++ other.initialStates)
                    else (state, sym) -> targets
            }
            Nfa(alphabet, states ++ other.states, trans ++ other.transitions, initialStates, other.acceptingStates)
        }
    }

    case class Dfa(alphabet: IndexedAlphabet,
        states: Set[State],
        transitions: Map[(State, Symbol), State],
        initialState: State,
        acceptingStates: Map[State, TokenID])

    def regexesToNfa(regexes: Seq[(Ast.Expression, TokenID)]): Nfa = {
        val alphabet = indexAlphabet(extractAlphabet(regexes.map(_._1)))
        val nfas = regexes.map { case (regex, tokenID) => regexToNfa(regex, tokenID, alphabet) }
        nfas.foldLeft( Nfa(alphabet, Set(), Map(), Set(), Map()) )(_ or _)
    }

    def regexToNfa(regex: Ast.Expression, tokenID: TokenID, alphabet: IndexedAlphabet): Nfa = regex match {
        case Ast.StringLit("") => {
            val s = new State
            Nfa(alphabet, Set(s), Map(), Set(s), Map(s -> tokenID))
        }
        case Ast.StringLit(str) => {
            val s = new State
            val a = new State
            val sym = lookup(alphabet, str(0))
            val first = Nfa(alphabet, Set(s,a), Map((s, sym) -> Set(a)), Set(s), Map(a -> tokenID))
            if (str.length > 1) {
                val rest = regexToNfa(Ast.StringLit(str.substring(1)), tokenID, alphabet)
                first concat rest
            } else first
        }
        case Ast.CharacterClass(ranges, inverted) => {
            val s = new State
            val a = new State
            val realRanges = if(inverted) invert(SortedSet(ranges : _*)).toSeq else ranges
            val trans = lookup(alphabet, realRanges).map { sym => (s, sym) -> Set(a)}.toMap
            Nfa(alphabet, Set(s, a), trans, Set(s), Map(a -> tokenID))
        }
        case Ast.KleeneStar(arg) =>
            val inner = regexToNfa(arg, tokenID, alphabet)
            val accepting = inner.acceptingStates ++ inner.initialStates.map(_ -> tokenID)
            (inner concat inner).copy(acceptingStates = accepting)
        case Ast.Or(lhs, rhs) =>
            regexToNfa(lhs, tokenID, alphabet) or regexToNfa(rhs, tokenID, alphabet)
        case Ast.Concattenation(lhs, rhs) =>
            regexToNfa(lhs, tokenID, alphabet) concat regexToNfa(rhs, tokenID, alphabet)
        case Ast.RuleName(_) =>
            sys.error("Rule name in regex should have been rejected.")
    }

    def nfaToDfa(nfa: Nfa): Dfa = {
        // TODO
        ???
    }

    /** A state doesn't have anything other than its identity. That is all we can do with a state is
     *  to check whether it's equal to another state.
     *
     *  Thus we represent states using an emtpy class, which only has its object identity.
     */
    class State {
        override def toString = BigInt(hashCode).toString(36)
    }

    /** An equivalence class is a pair of characters, representing an inclusive range of chatacters
     *  that are equivalent for the purposes of the automaton. That is if, for example, all characters
     *  between a and z would lead to the same transitions in all cases, ('a','z') would constitute an
     *  equivalence class.
     *
     *  This way, if a regex contains a character range containing hundreds of characters, we don't
     *  need hundreds of transitions in our automaton.
     */
    type EquivalenceClass = (Char, Char)

    type Alphabet = SortedSet[EquivalenceClass]
    type IndexedAlphabet = SortedMap[EquivalenceClass, Symbol]
    val emptyAlphabet: Alphabet = SortedSet[EquivalenceClass]()

    /** Translate a character to a symbol, which is its index in the list of equivalence classes. */
    def lookup(alphabet: IndexedAlphabet, char: Char): Symbol = {
        val ((start, end), id) = alphabet.until(succ(char), succ(char)).max
        if (start <= char && char <= end) id
        else {
            sys.error(s"Character $char not found in alphabet $alphabet.")
        }
    }

    /** Translate a range of characters to one or more symbols, which are the corresponding indices
     *  in the list of equivalence classes.
     */
    def lookup(alphabet: IndexedAlphabet, ranges: Seq[(Char, Char)]): Seq[Symbol] = {
        ranges.flatMap { case (from, to) =>
            val ((fromStart, fromEnd), fromId) = alphabet.until(succ(from), succ(from)).max
            val ((toStart, toEnd), toId) = alphabet.until(succ(to), succ(to)).max
            if (fromStart <= from && from <= fromEnd && toStart <= to && to <= toEnd) fromId.to(toId)
            else sys.error(s"Characters $from or $to not found in alphabet.")
        }
    }

    private def pred(c: Char): Char = (c - 1).toChar
    private def succ(c: Char): Char = Seq(c, (c + 1).toChar).max

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

    def extractAlphabet(regex: Ast.Expression): Alphabet = regex match {
        case Ast.StringLit(str) =>
            SortedSet(str.map(c => (c,c)) : _*)
        case Ast.CharacterClass(ranges, false) =>
            SortedSet(ranges : _*)
        case Ast.CharacterClass(ranges, true) =>
            invert(SortedSet(ranges : _*))
        case Ast.KleeneStar(arg) =>
            extractAlphabet(arg)
        case Ast.Or(lhs, rhs) =>
            mergeAlphabets(extractAlphabet(lhs), extractAlphabet(rhs), emptyAlphabet)
        case Ast.Concattenation(lhs, rhs) =>
            mergeAlphabets(extractAlphabet(lhs), extractAlphabet(rhs), emptyAlphabet)
        case Ast.RuleName(_) =>
            sys.error("Rule name in regex should have been rejected.")
    }

    def extractAlphabet(regexes: Seq[Ast.Expression]): Alphabet = {
        regexes.map(extractAlphabet).foldLeft(emptyAlphabet) {
            (acc, alpha) => mergeAlphabets(acc, alpha, emptyAlphabet)
        }
    }

    def indexAlphabet(alphabet: Alphabet): IndexedAlphabet = {
        SortedMap(alphabet.zipWithIndex.toSeq : _*)
    }

    def regexesToDfa(regexes: List[(Ast.Expression, TokenID)]): Dfa = {
        nfaToDfa(regexesToNfa(regexes))
    }
}