package parigen.plang

object PLang {
    case class Module(name: String, body: Seq[Definition])

    sealed abstract class Statement
    case class While(cond: Expression, body: Seq[Statement]) extends Statement
    case class Return(expr: Expression) extends Statement
    case class Assignment(lexpr: Expression, expr: Expression) extends Statement
    case class ExpressionStatement(expr: Expression) extends Statement

    sealed abstract class Definition extends Statement
    case class VarDef(name: String, typ: Type, init: Expression) extends Definition
    case class FunDef(name: String, params: Seq[(String, Type)], returnType: Type, body: Seq[Statement]) extends Definition
    case class ObjectTypeDef(name: String, members: Seq[(String, Type)]) extends Definition
    case class EnumDef(name: String, members: Seq[(String, Type)]) extends Definition

    sealed abstract class Type
    case object IntType extends Type
    case object StringType extends Type
    case object CharType extends Type
    case class UserDefinedType(name: String) extends Type

    sealed abstract class Expression
    case class Var(name: String) extends Expression
    case class IntLit(value: Int) extends Expression
    case class StringLit(value: String) extends Expression
    case class CharLit(value: Char) extends Expression
    case class ObjectLit(structName: String, members: Seq[(String, Expression)]) extends Expression
    case class Add(lhs: Expression, rhs: Expression) extends Expression
    case class Eq(lhs: Expression, rhs: Expression) extends Expression
    case class Lt(lhs: Expression, rhs: Expression) extends Expression
    case class Gt(lhs: Expression, rhs: Expression) extends Expression
    case class LtEq(lhs: Expression, rhs: Expression) extends Expression
    case class GtEq(lhs: Expression, rhs: Expression) extends Expression
    case class FunCall(fun: Expression, args: Seq[Expression]) extends Expression
    case class Subscript(indexable: Expression, index: Expression) extends Expression
    case class MemberAccess(receiver: Expression, memberName: String) extends Expression
}