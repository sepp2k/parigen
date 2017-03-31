package parigen

import scala.collection.mutable

package ast {
    case class Grammar(rules: Seq[Rule]) {
        override def toString = rules.mkString("\n")
    }

    case class RuleFlags(tokenRule: Boolean = false) {
        override def toString = if(tokenRule) "token " else ""
    }

    case class Rule(name: String, flags: RuleFlags, exp: Expression) {
        override def toString = s"$flags$name: $exp;"
    }

    sealed abstract class Expression
    case class RuleName(name: String) extends Expression {
        override def toString = name
    }
    case class StringLit(string: String) extends Expression {
        override def toString = s""""$string"""" // Fix VS code syntax highlighting: "
    }
    case class CharacterClass(ranges: Seq[(Char, Char)], negated: Boolean) extends Expression {
        override def toString = {
            val caret = if(negated) "^" else ""
            val charRanges = ranges.map {case (from, to) => s"$from-$to"}.mkString
            s"[$caret$charRanges]"
        }
    }

    object CharacterClass {
        def parse(str: String): CharacterClass = {
            var i = 1
            var negated = false
            if(str.charAt(i) == '^') {
                i += 1
                negated = true;
            }
            val ranges = new mutable.ArrayBuffer[(Char, Char)]
            while(i < str.length - 1) {
                var ch = str.charAt(i)
                if(ch == '\\') {
                    i += 1
                    ch = str.charAt(i)
                }
                i += 1
                if(str.charAt(i) == '-' && i + 1 < str.length - 1) {
                    i += 1
                    var ch2 = str.charAt(i)
                    if(ch2 == '\\') {
                        i += 1
                        ch2 = str.charAt(i)
                    }
                    i += 1
                    ranges += ch -> ch2
                } else {
                    ranges += ch -> ch
                }
            }
            CharacterClass(ranges, negated)
        }
    }

    case class Or(lhs: Expression, rhs: Expression) extends Expression {
        override def toString = s"($lhs | $rhs)"
    }
    case class Concattenation(lhs: Expression, rhs: Expression) extends Expression {
        override def toString = s"($lhs $rhs)"
    }
    case class KleeneStar(arg: Expression) extends Expression {
        override def toString = s"($arg*)"
    }
    case object Epsilon extends Expression {
        override def toString = "ε"
    }
}