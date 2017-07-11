package parigen.lexer_generator

import parigen._
import Automaton._
import Dfa._
import TokenInfo.TokenID
import scala.collection.mutable

case class Dfa(
    alphabet: IndexedAlphabet,
    states: Seq[State],
    transitions: Transitions,
    initialState: State,
    acceptingStates: Map[State, TokenID]
) extends Automaton {
    val name = "DFA"
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
            case None =>
                val thisState = new State
                stateMappings(current) = thisState
                val outgoing: Map[Symbol, Set[State]] = current.flatMap { state =>
                    nfa.transitionsFrom(state).toSeq.flatMap { case (symbol, newStates) =>
                        newStates.map { newState =>
                            (symbol, newState)
                        }
                    }
                }.groupBy(_._1).mapValues(_.map(_._2))
                outgoing.flatMap { case (symbol, states) =>
                    determinizeTrans(nfa, states, stateMappings)
                } + (thisState -> outgoing.mapValues(stateMappings(_)))
        }
    }

    def fromNfa(nfa: Nfa): Dfa = {
        val stateMappings = mutable.Map[Set[State], State]()
        val trans = determinizeTrans(nfa, nfa.initialStates, stateMappings)
        val acceptingStates = stateMappings.flatMap { case (nfaStates, dfaState) =>
            val tokenIDs = nfaStates.flatMap(nfa.isAccepting)
            if (tokenIDs.isEmpty) None
            else {
                // Literals have smaller token IDs than token-definitions and token definitions
                // have increasing IDs according to their position in the grammar.
                // So by taking the min, we implement the rules that literals take precedence over
                // token-definitions (so keywords would take precedence over an ID rule) and earlier
                // deifnitions take precedence over later ones.
                Some(dfaState -> tokenIDs.min)
            }
        }.toMap
        Dfa(nfa.alphabet, stateMappings.values.toVector, trans, stateMappings(nfa.initialStates), acceptingStates)
    }
}