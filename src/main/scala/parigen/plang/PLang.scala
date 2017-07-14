package parigen.plang

import scala.language.implicitConversions

object PLang {
    case class Module(name: String, body: Definition*)

    sealed abstract class Statement
    case class While(cond: Expression, body: Statement*) extends Statement
    case class Switch(exp: Expression, default: Option[Seq[Statement]], body: (Seq[Constant], Seq[Statement])*) extends Statement
    case class Return(expr: Expression) extends Statement
    case class Assignment(lexpr: Expression, expr: Expression) extends Statement
    case class ExpressionStatement(expr: Expression) extends Statement

    sealed abstract class Definition extends Statement
    sealed abstract class MemberDef extends Definition
    case class VarDef(name: String, typ: Type, init: Expression) extends MemberDef
    case class FunDef(name: String, returnType: Type, params: Seq[(String, Type)], body: Seq[Statement]) extends MemberDef
    case class ClassDef(name: String, params: Seq[(String, Type)], body: Seq[MemberDef]) extends Definition
    case class EnumDef(name: String, members: String*) extends Definition

    def funDef(name: String, returnType: Type, params: (String, Type)*)(body: Statement*): FunDef = {
        FunDef(name, returnType, params, body)
    }

    def classDef(name: String, params: (String, Type)*)(body: MemberDef*): ClassDef = {
        ClassDef(name, params, body)
    }

    sealed abstract class Type
    case object IntType extends Type
    case object StringType extends Type
    case object CharType extends Type
    case class UserDefinedType(name: String) extends Type

    sealed abstract class Expression {
        def +(other: Expression) = Add(this, other)
        def ===(other: Expression) = Eq(this, other)
        def :=(other: Expression) = Assignment(this, other)
        def <(other: Expression) = Lt(this, other)
        def >(other: Expression) = Gt(this, other)
        def <=(other: Expression) = LtEq(this, other)
        def >=(other: Expression) = GtEq(this, other)
        def apply(args: Expression*) = FunCall(this, args)
        def sub(index: Expression) = Subscript(this, index)
        def member(memberName: String) = MemberAccess(this, memberName)
    }
    case class Var(name: String) extends Expression
    case object This extends Expression
    case class Instantiate(className: String, params: Expression*) extends Expression
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

    implicit def makeLiteral(value: Int) = IntLit(value)
    implicit def makeLiteral(value: String) = StringLit(value)
    implicit def makeLiteral(value: Char) = CharLit(value)
    implicit def makeLiteral(value: Boolean) = BoolLit(value)
}