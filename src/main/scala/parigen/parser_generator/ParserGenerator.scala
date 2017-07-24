package parigen.parser_generator

import parigen.TokenInfo
import parigen.plang.PLang
import parigen.SimplifiedGrammar

object ParserGenerator {
    type FirstSet = Set[Option[TokenInfo.TokenID]]
    type FirstSets = Map[String, FirstSet]
    type FollowSet = Set[TokenInfo.TokenID]
    type FollowSets = Map[String, FollowSet]
    case class Parser(firstSets: FirstSets, followSets: FollowSets, parsingTable: ParsingTable, code: PLang.Module)

    def generateParser(grammar: SimplifiedGrammar.Grammar): Parser = {
        val pg = new ParserGenerator(grammar)
        Parser(pg.firstSets, null, null, null)
    }
}

import ParserGenerator._

class ParserGenerator(grammar: SimplifiedGrammar.Grammar) {
    def firstSets: FirstSets = {
        grammar.nonTerminals.map { case (name, body) =>
            name -> firstSet(body)
        }
    }

    def firstSet(exp: SimplifiedGrammar.Expression): FirstSet = {
        exp.alternatives.flatMap {
            case SimplifiedGrammar.Concatenation(Seq()) => Seq(None)
            case concatenation => firstSetForConcat(concatenation.items)
        }.toSet
    }

    def firstSetForConcat(items: Seq[SimplifiedGrammar.Item]): FirstSet = items match {
        case Seq() => Set(None)
        case Seq(SimplifiedGrammar.Terminal(name), _ @ _*) =>
            Set(Some(grammar.terminals(name).id))
        case Seq(SimplifiedGrammar.NonTerminal(name), tail @ _*) =>
            val set = firstSet(grammar.nonTerminals(name))
            if (set.contains(None)) {
                (set - None) ++ firstSetForConcat(tail)
            } else {
                set
            }
    }
}