package parigen.plang

object PLang {
    case class Module(name: String, body: Seq[Definition])

    sealed abstract class Statement
    case class While(cond: Expression, body: Seq[Statement]) extends Statement
    case class Switch(exp: Expression, body: Seq[(Constant, Seq[Statement])]) extends Statement
    case class Return(expr: Expression) extends Statement
    case class Assignment(lexpr: Expression, expr: Expression) extends Statement
    case class ExpressionStatement(expr: Expression) extends Statement

    sealed abstract class Definition extends Statement
    sealed abstract class MemberDef extends Definition
    case class VarDef(name: String, typ: Type, init: Expression) extends MemberDef
    case class FunDef(name: String, params: Seq[(String, Type)], returnType: Type, body: Seq[Statement]) extends MemberDef
    case class ClassDef(name: String, params: Seq[(String, Type)], body: Seq[MemberDef]) extends Definition
    case class EnumDef(name: String, members: Seq[String]) extends Definition

    sealed abstract class Type
    case object IntType extends Type
    case object StringType extends Type
    case object CharType extends Type
    case class UserDefinedType(name: String) extends Type

    sealed abstract class Expression
    case class Var(name: String) extends Expression
    case object This extends Expression
    case class Instantiate(className: String, params: Seq[Expression]) extends Expression
    case class Add(lhs: Expression, rhs: Expression) extends Expression
    case class Eq(lhs: Expression, rhs: Expression) extends Expression
    case class Lt(lhs: Expression, rhs: Expression) extends Expression
    case class Gt(lhs: Expression, rhs: Expression) extends Expression
    case class LtEq(lhs: Expression, rhs: Expression) extends Expression
    case class GtEq(lhs: Expression, rhs: Expression) extends Expression
    case class FunCall(fun: Expression, args: Seq[Expression]) extends Expression
    case class Subscript(indexable: Expression, index: Expression) extends Expression
    case class MemberAccess(receiver: Expression, memberName: String) extends Expression

    sealed abstract class Constant extends Expression
    case class IntLit(value: Int) extends Constant
    case class BoolLit(value: Boolean) extends Constant
    case class StringLit(value: String) extends Constant
    case class CharLit(value: Char) extends Constant
    case class EnumMember(enumName: String, memberName: String) extends Constant
}