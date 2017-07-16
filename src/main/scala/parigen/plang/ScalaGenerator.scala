package parigen.plang

import java.io.PrintStream

class ScalaGenerator(packageName: String, out: PrintStream, indentationWidth: Int) {
    def generate(mod: PLang.Module): Unit = {
        out.println(s"package $packageName")
        out.println(s"object ${mod.name} {")
        mod.body.foreach {definition =>
            generateStatement(definition, indentationWidth)
        }
        out.println("}")
    }

    def generateStatement(statement: PLang.Statement, indentation: Int): Unit = {
        def println(line: String, extraIndent: Int = 0): Unit = {
            out.println(s"${" " * (indentation + extraIndent * indentationWidth)}$line")
        }
        statement match {
            case PLang.VarDef(name, typ, init) =>
                println(s"var $name: ${translateType(typ)} = ${translateExp(init)}")
            case PLang.Assignment(lhs, rhs) =>
                println(s"${translateExp(lhs)} = ${translateExp(rhs)}")
            case PLang.FunDef(name, returnType, params, body) =>
                val paramList = params.map { case (param, typ) => s"$param: ${translateType(typ)}"}.mkString(", ")
                println(s"def $name($paramList): ${translateType(returnType)} = {")
                body.foreach { statement =>
                    generateStatement(statement, indentation + indentationWidth)
                }
                // Add error at the end of the function to avoid type errors by the Scala compiler
                // Since PLai functions should always reach a return statement, this error should never be reached
                println("sys.error(\"Missing return statement\")", extraIndent = 1)
                println(s"}")
            case PLang.ClassDef(name, params, body) =>
                val paramsString = params.map { case (memberName, typ) => s"$memberName: ${translateType(typ)}"}.mkString(", ")
                println(s"case class $name($paramsString) {")
                body.foreach(generateStatement(_, indentation + indentationWidth))
                println("}")
            case PLang.EnumDef(name, members @ _*) =>
                println(s"sealed abstract class $name")
                members.foreach { member =>
                    println(s"case object ${name}_$member extends $name")
                }
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
                println(s"${translateExp(exp)} match {")
                body.foreach { case (consts, caseBody) =>
                    consts.foreach(const => println(s"case ${translateExp(const)} =>"))
                    caseBody.foreach(generateStatement(_, indentation + indentationWidth))
                }
                default.foreach { defaultBody =>
                    println("case _ =>")
                    defaultBody.foreach(generateStatement(_, indentation + indentationWidth))
                }
                println("}")
            case PLang.Return(exp) =>
                println(s"return ${translateExp(exp)}")
            case _ =>
                println(s"// TODO: $statement")
        }
    }

    def translateType(typ: PLang.Type): String = {
        typ match {
            case PLang.StringType => "String"
            case PLang.IntType => "Int"
            case PLang.CharType => "Char"
            case PLang.BoolType => "Boolean"
            case PLang.UserDefinedType(name) => name
        }
    }

    val IsAsciiPrintable = """[\\\[\]\-_,.;:#'"+*~^!"$%&/(){}=?`@<>|a-zA-Z0-9]""".r

    def escapeChar(c: Char) = c match {
        case '\\' => "\\\\"
        case '"' => "\\\""
        case '\'' => "\\'"
        case '\n' => "\\n"
        case '\r' => "\\r"
        case '\t' => "\\t"
        case IsAsciiPrintable() => c.toString
        case c => "\\u%04x".format(c.toInt)
    }

    def translateExp(exp: PLang.Expression): String = {
        exp match {
            case PLang.Var(name) => name
            case PLang.This => "this"
            case PLang.IntLit(value) => value.toString
            case PLang.StringLit(value) => s""""${value.map(escapeChar)}""""
            case PLang.BoolLit(value) => value.toString
            case PLang.CharLit(value) => s"'${escapeChar(value)}'"
            case PLang.Instantiate(className, args @ _*) => args.map(translateExp).mkString(s"$className(", ", ", ")")
            case PLang.Add(lhs, rhs) => s"(${translateExp(lhs)} + ${translateExp(rhs)})"
            case PLang.Sub(lhs, rhs) => s"(${translateExp(lhs)} - ${translateExp(rhs)})"
            case PLang.Eq(lhs, rhs) => s"(${translateExp(lhs)} == ${translateExp(rhs)})"
            case PLang.Lt(lhs, rhs) => s"(${translateExp(lhs)} < ${translateExp(rhs)})"
            case PLang.Gt(lhs, rhs) => s"(${translateExp(lhs)} > ${translateExp(rhs)})"
            case PLang.LtEq(lhs, rhs) => s"(${translateExp(lhs)} <= ${translateExp(rhs)})"
            case PLang.GtEq(lhs, rhs) => s"(${translateExp(lhs)} >= ${translateExp(rhs)})"
            case PLang.And(lhs, rhs) => s"(${translateExp(lhs)} && ${translateExp(rhs)})"
            case PLang.Or(lhs, rhs) => s"(${translateExp(lhs)} || ${translateExp(rhs)})"
            case PLang.EnumMember(enumName, memberName) => s"${enumName}_$memberName"
            case PLang.MemberAccess(receiver, memberName) => s"${translateExp(receiver)}.$memberName"
            case PLang.StringSubscript(string, index) => s"${translateExp(string)}.charAt(${translateExp(index)})"
            case PLang.FunCall(fun, args) => s"${translateExp(fun)}(${args.map(translateExp).mkString(", ")})"
            case _ => s"/* TODO: $exp */"
        }
    }
}