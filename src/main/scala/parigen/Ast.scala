package parigen

package ast {
    case class Grammar(rules: Map[String, Expression]) {
        override def toString = rules.map { case (name, exp) => s"$name: $exp;" }.mkString("\n")
    }

    sealed abstract class Expression
    case class RuleName(name: String) extends Expression {
        override def toString = name
    }
    case class StringLit(string: String) extends Expression {
        override def toString = s""""$string"""" // Fix VS code syntax highlighting: "
    }
    case class Or(lhs: Expression, rhs: Expression) extends Expression {
        override def toString = s"($lhs | $rhs)"
    }
    case class Seq(lhs: Expression, rhs: Expression) extends Expression {
        override def toString = s"($lhs $rhs)"
    }
    case class KleeneStar(arg: Expression) extends Expression {
        override def toString = s"($arg*)"
    }
    case object Epsilon extends Expression {
        override def toString = "ε"
    }
}