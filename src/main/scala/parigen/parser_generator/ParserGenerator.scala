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
        val firstSets = pg.firstSets
        Parser(firstSets, pg.followSets(firstSets), null, null)
    }
}

import ParserGenerator._

class ParserGenerator(grammar: SimplifiedGrammar.Grammar) {
    def firstSets: FirstSets = {
        grammar.nonTerminals.foldLeft(Map[String, FirstSet]()) { case (acc, (name, body)) =>
            acc + (name -> firstSet(body, acc))
        }
    }

    def firstSet(exp: SimplifiedGrammar.Expression, alreadyComputed: FirstSets): FirstSet = {
        exp.alternatives.flatMap {
            case SimplifiedGrammar.Concatenation(Seq()) => Seq(None)
            case concatenation => firstSetForConcat(concatenation.items, alreadyComputed)
        }.toSet
    }

    def firstSetForConcat(items: Seq[SimplifiedGrammar.Item], alreadyComputed: FirstSets): FirstSet = items match {
        case Seq() => Set(None)
        case Seq(SimplifiedGrammar.Terminal(name), _ @ _*) =>
            Set(Some(grammar.terminals(name).id))
        case Seq(SimplifiedGrammar.NonTerminal(name), tail @ _*) =>
            val set = alreadyComputed.getOrElse(name, firstSet(grammar.nonTerminals(name), alreadyComputed))
            if (set.contains(None)) {
                (set - None) ++ firstSetForConcat(tail, alreadyComputed)
            } else {
                set
            }
    }

    /** Return a set containing the name of the given non-terminal as well as those of any non-terminals that
      * can appear in its tail position */
    def trailingNonTerms(name: String, visited: Set[String] = Set()): Set[String] = {
        if (visited.contains(name)) Set()
        else {
            grammar.nonTerminals(name).alternatives.flatMap { concat =>
                concat.items.lastOption match {
                    case Some(SimplifiedGrammar.NonTerminal(trailing)) => trailingNonTerms(trailing, visited + name) + trailing
                    case Some(SimplifiedGrammar.Terminal(_)) | None => None
                }
            }.toSet + name
        }
    }

    def followSets(firstSets: FirstSets): FollowSets = {
        grammar.nonTerminals.foldLeft(Map[String, FollowSet]()) { case (acc, (_, body)) =>
            body.alternatives.foldLeft(acc) { case (acc, concat) =>
                concat.items.zip(concat.items.tails.toIterable.tail).foldLeft(acc) {
                    case (acc, (SimplifiedGrammar.Terminal(term), _)) => acc
                    case (acc, (SimplifiedGrammar.NonTerminal(name), tail)) =>
                        val follow = firstSetForConcat(tail, firstSets).flatMap(x => x)
                        val newEntries = trailingNonTerms(name).map {
                            name => name -> (acc.getOrElse(name, Set()) ++ follow)
                        }
                        acc ++ newEntries
                }
            }
        }
    }
}