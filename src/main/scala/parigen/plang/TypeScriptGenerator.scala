package parigen.plang

import java.io.PrintStream

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
            case PLang.FunDef(name, params, returnType, body) =>
                val paramList = params.map { case (param, typ) => s"$param: ${translateType(typ)}"}.mkString(", ")
                println(s"function $name($paramList): ${translateType(returnType)} {")
                body.foreach { statement =>
                    generateStatement(statement, indentation + indentationWidth)
                }
                println(s"}")
            case PLang.ObjectTypeDef(name, members) =>
                println(s"type $name = {")
                members.foreach { case (memberName, typ) =>
                    println(s"$memberName: ${translateType(typ)},", extraIndent = 1)
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
            case PLang.UserDefinedType(name) => name
        }
    }

    def translateExp(exp: PLang.Expression): String = {
        exp match {
            case PLang.Var(name) => name
            case PLang.IntLit(value) => value.toString
            case PLang.StringLit(value) => s""""$value""""
            case PLang.CharLit('\'') => s"'\\''"
            case PLang.CharLit(value) => s"'$value'"
            case PLang.ObjectLit(_, members) => members.map {case (name, value) => s"$name: ${translateExp(value)}"}.mkString("{", ", ", "}")
            case PLang.Add(lhs, rhs) => s"(${translateExp(lhs)} + ${translateExp(rhs)}"
            case PLang.Eq(lhs, rhs) => s"(${translateExp(lhs)} == ${translateExp(rhs)}"
            case PLang.Lt(lhs, rhs) => s"(${translateExp(lhs)} < ${translateExp(rhs)}"
            case PLang.Gt(lhs, rhs) => s"(${translateExp(lhs)} > ${translateExp(rhs)}"
            case PLang.LtEq(lhs, rhs) => s"(${translateExp(lhs)} <= ${translateExp(rhs)}"
            case PLang.GtEq(lhs, rhs) => s"(${translateExp(lhs)} >= ${translateExp(rhs)}"
            case _ => s"/* TODO: $exp */"
        }
    }
}