package parigen.plang

import java.io.PrintStream
import scala.collection.mutable

class TypeScriptGenerator(out: PrintStream, indentationWidth: Int) {
    def generate(mod: PLang.Module): Unit = {
        out.println(s"// module ${mod.name}")
        mod.body.foreach {definition =>
            generateStatement(definition, 0)
        }
    }

    def generateStatement(statement: PLang.Statement, indentation: Int): Unit = {
        def println(line: String, extraIndent: Int = 0): Unit = {
            out.println(s"${" " * (indentation + extraIndent * indentationWidth)}$line")
        }
        statement match {
            case PLang.VarDef(name, typ, init) =>
                println(s"let $name: ${translateType(typ)} = ${translateExp(init)};")
            case PLang.Assignment(lhs, rhs) =>
                println(s"${translateExp(lhs)} = ${translateExp(rhs)};")
            case PLang.FunDef(name, returnType, params, body) =>
                val paramList = params.map { case (param, typ) => s"$param: ${translateType(typ)}"}.mkString(", ")
                println(s"function $name($paramList): ${translateType(returnType)} {")
                body.foreach { statement =>
                    generateStatement(statement, indentation + indentationWidth)
                }
                println(s"}")
            case PLang.ClassDef(name, params, body) =>
                val paramStrings = params.map { case (memberName, typ) => s"$memberName: ${translateType(typ)}"}
                val inits = mutable.ArrayBuffer[(String, PLang.Expression)]()
                println(s"class $name {")
                body.foreach {
                    case PLang.FunDef(name, returnType, params, body) =>
                        val paramList = params.map { case (param, typ) => s"$param: ${translateType(typ)}"}.mkString(", ")
                        println(s"$name($paramList): ${translateType(returnType)} {", extraIndent = 1)
                        body.foreach { statement =>
                            generateStatement(statement, indentation + 2 * indentationWidth)
                        }
                        println(s"}", extraIndent = 1)
                    case PLang.VarDef(name, typ, init) =>
                        println(s"$name: ${translateType(typ)};", extraIndent = 1)
                        inits += name -> init
                }
                paramStrings.foreach { paramString =>
                    println(s"$paramString;", extraIndent = 1)
                }
                println(s"constructor(${paramStrings.mkString(", ")}) {", extraIndent = 1)
                params.foreach { case (memberName, _) =>
                    println(s"this.$memberName = $memberName;", extraIndent = 2)
                }
                inits.foreach { case (memberName, init) =>
                    println(s"this.$memberName = ${translateExp(init)};", extraIndent = 2)
                }
                println("}", extraIndent = 1)
                println("}")
            case PLang.EnumDef(name, members @ _*) =>
                println(s"enum $name {")
                members.foreach { member =>
                    println(s"$member,", extraIndent = 1)
                }
                println("}")
            case PLang.While(cond, body @ _*) =>
                println(s"while(${translateExp(cond)}) {")
                body.foreach(generateStatement(_, indentation + indentationWidth))
                println("}")
            case PLang.IfThenElse(cond, thenCase, elseCase) =>
                println(s"if (${translateExp(cond)}) {")
                thenCase.foreach(generateStatement(_, indentation + indentationWidth))
                println("} else {")
                elseCase.foreach(generateStatement(_, indentation + indentationWidth))
                println("}")
            case PLang.Switch(exp, default, body @ _*) =>
                println(s"switch(${translateExp(exp)}) {")
                body.foreach { case (consts, caseBody) =>
                    consts.foreach(const => println(s"case ${translateExp(const)}:"))
                    caseBody.foreach(generateStatement(_, indentation + indentationWidth))
                    // Don't generate breaks after returns to avoid dead code errors
                    if(!caseBody.last.isInstanceOf[PLang.Return]) println("break;", extraIndent = 1)
                }
                default.foreach { defaultBody =>
                    println("default:")
                    defaultBody.foreach(generateStatement(_, indentation + indentationWidth))
                    // Don't generate breaks after returns to avoid dead code errors
                    if(!defaultBody.last.isInstanceOf[PLang.Return]) println("break;", extraIndent = 1)
                }
                println("}")
            case PLang.Return(exp) =>
                println(s"return ${translateExp(exp)};")
            case _ =>
                println(s"// TODO: $statement")
        }
    }

    def translateType(typ: PLang.Type): String = {
        typ match {
            case PLang.StringType => "string"
            case PLang.IntType => "number"
            case PLang.CharType => "string"
            case PLang.BoolType => "boolean"
            case PLang.UserDefinedType(name) => name
        }
    }

    def translateExp(exp: PLang.Expression): String = {
        exp match {
            case PLang.Var(name) => name
            case PLang.This => "this"
            case PLang.IntLit(value) => value.toString
            case PLang.StringLit(value) => s""""$value""""
            case PLang.BoolLit(value) => value.toString
            case PLang.CharLit('\'') => s"'\\''"
            case PLang.CharLit('\\') => s"'\\\\'"
            case PLang.CharLit(value) if value.isControl => "'\\%04x'".format(value.toInt)
            case PLang.CharLit(value) => s"'$value'"
            case PLang.Instantiate(className, args @ _*) => args.map(translateExp).mkString(s"new $className(", ", ", ")")
            case PLang.Add(lhs, rhs) => s"(${translateExp(lhs)} + ${translateExp(rhs)})"
            case PLang.Eq(lhs, rhs) => s"(${translateExp(lhs)} == ${translateExp(rhs)})"
            case PLang.Lt(lhs, rhs) => s"(${translateExp(lhs)} < ${translateExp(rhs)})"
            case PLang.Gt(lhs, rhs) => s"(${translateExp(lhs)} > ${translateExp(rhs)})"
            case PLang.LtEq(lhs, rhs) => s"(${translateExp(lhs)} <= ${translateExp(rhs)})"
            case PLang.GtEq(lhs, rhs) => s"(${translateExp(lhs)} >= ${translateExp(rhs)})"
            case PLang.And(lhs, rhs) => s"(${translateExp(lhs)} && ${translateExp(rhs)})"
            case PLang.Or(lhs, rhs) => s"(${translateExp(lhs)} || ${translateExp(rhs)})"
            case PLang.EnumMember(enumName, memberName) => s"$enumName.$memberName"
            case PLang.MemberAccess(receiver, memberName) => s"${translateExp(receiver)}.$memberName"
            case PLang.StringSubscript(string, index) => s"${translateExp(string)}.charAt(${translateExp(index)})"
            case PLang.FunCall(fun, args) => s"${translateExp(fun)}(${args.map(translateExp).mkString(", ")})"
            case _ => s"/* TODO: $exp */"
        }
    }
}