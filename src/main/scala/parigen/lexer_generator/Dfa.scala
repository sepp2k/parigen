package parigen.lexer_generator

import parigen._
import Automaton._
import Dfa._
import TokenInfo.TokenID
import scala.collection.mutable

case class Dfa(
    alphabet: IndexedAlphabet,
    transitions: Transitions,
    initialState: State,
    acceptingStates: Map[State, TokenID]
) extends Automaton {
    def flatTransitions =
        for((state, outgoing) <- transitions; (sym, target) <- outgoing)
            yield (state, sym, target)

    def isInitialState(state: State) = state == initialState

    def isAccepting(state: State) = acceptingStates.get(state)
}

object Dfa {
    type StateMappings = mutable.Map[Set[State], State]
    type Transitions = Map[State, Map[Symbol, State]]

    def determinizeTrans(nfa: Nfa, current: Set[State], stateMappings: StateMappings): Transitions = {
        stateMappings.get(current) match {
            case Some(_) => Map()
            case None => ???
        }
    }

    def fromNfa(nfa: Nfa): Dfa = {
        // TODO
        ???
    }

    def fromRegexes(regexes: List[(Ast.Expression, TokenID)]): Dfa = {
        fromNfa(Nfa.fromRegexes(regexes))
    }
}