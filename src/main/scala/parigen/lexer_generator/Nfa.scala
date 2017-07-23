package parigen.lexer_generator

import parigen._
import Automaton._
import TokenInfo.TokenID
import scala.collection.immutable.SortedSet

case class Nfa (
    alphabet: IndexedAlphabet,
    transitions: Map[State, Map[Symbol, Set[State]]],
    initialStates: Set[State],
    acceptingStates: Map[State, TokenID]
) extends Automaton {
    val name = "NFA"
    def flatTransitions =
        for((state, outgoing) <- transitions; (sym, targets) <- outgoing; target <- targets)
            yield (state, sym, target)

    def isAccepting(state: State) = acceptingStates.get(state)

    def isInitialState(state: State) = initialStates(state)

    def transitionsFrom(state: State) = transitions.getOrElse(state, Map())

    def or(other: Nfa) = {
        require(alphabet == other.alphabet)
        Nfa(alphabet, util.Map.mergeNestedSetMaps(transitions, other.transitions),
            initialStates ++ other.initialStates, acceptingStates ++ other.acceptingStates)
    }

    def concat(other: Nfa) = {
        require(alphabet == other.alphabet)
        val trans = transitions.map {
            case (state, outgoing) =>
                state -> outgoing.map {
                    case (sym, targets) =>
                        val newTargets = targets.flatMap { target =>
                            if (acceptingStates.isDefinedAt(target)) {
                                if (transitionsFrom(target).isEmpty) other.initialStates
                                else other.initialStates + target
                            } else List(target)
                        }
                        sym -> newTargets
                }
        }
        Nfa(alphabet, util.Map.mergeNestedSetMaps(trans, other.transitions), initialStates, other.acceptingStates)
    }
}

object Nfa {
    def fromRegexes(regexes: Seq[(Ast.Expression, TokenID)]): Nfa = {
        val alphabet = indexAlphabet(extractAlphabet(regexes.map(_._1)))
        val nfas = regexes.map { case (regex, tokenID) => fromRegex(regex, tokenID, alphabet) }
        nfas.foldLeft( Nfa(alphabet, Map(), Set(), Map()) )(_ or _)
    }

    def fromRegex(regex: Ast.Expression, tokenID: TokenID, alphabet: IndexedAlphabet): Nfa = regex match {
        case Ast.StringLit("") => {
            val s = new State
            Nfa(alphabet, Map(), Set(s), Map(s -> tokenID))
        }
        case Ast.StringLit(str) => {
            val s = new State
            val a = new State
            val sym = lookup(alphabet, str(0))
            val first = Nfa(alphabet, Map(s -> Map(sym -> Set(a))), Set(s), Map(a -> tokenID))
            if (str.length > 1) {
                val rest = fromRegex(Ast.StringLit(str.substring(1)), tokenID, alphabet)
                first concat rest
            } else first
        }
        case Ast.CharacterClass(ranges, inverted) => {
            val s = new State
            val a = new State
            val realRanges = if(inverted) invert(SortedSet(ranges : _*)).toSeq else ranges
            val trans = Map(s -> lookup(alphabet, realRanges).map { sym =>  sym -> Set(a)}.toMap)
            Nfa(alphabet, trans, Set(s), Map(a -> tokenID))
        }
        case Ast.KleeneStar(arg) =>
            val inner = fromRegex(arg, tokenID, alphabet)
            val accepting = inner.acceptingStates ++ inner.initialStates.map(_ -> tokenID)
            (inner concat inner).copy(acceptingStates = accepting)
        case Ast.Or(lhs, rhs) =>
            fromRegex(lhs, tokenID, alphabet) or fromRegex(rhs, tokenID, alphabet)
        case Ast.Concatenation(lhs, rhs) =>
            fromRegex(lhs, tokenID, alphabet) concat fromRegex(rhs, tokenID, alphabet)
        case Ast.RuleName(_) =>
            sys.error("Rule name in regex should have been rejected.")
    }
}