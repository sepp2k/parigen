package parigen.lexer_generator

import parigen.plang.PLang

object CodeGenerator {
    def fromDfa(dfa: Dfa): PLang.Module = {
        PLang.Module("lexer", Seq(
            PLang.ObjectTypeDef("Lexer", Seq(
                "source" -> PLang.StringType,
                "index" -> PLang.IntType
            )),
            PLang.ObjectTypeDef("Lexer", Seq(
                "kind" -> PLang.StringType,
                "line" -> PLang.IntType,
                "from" -> PLang.IntType,
                "to" -> PLang.IntType
            )),
            PLang.FunDef("makeLexer", Seq("source" -> PLang.StringType), PLang.UserDefinedType("Lexer"), Seq(
                PLang.Return(PLang.ObjectLit("Lexer", Seq(
                    "source" -> PLang.Var("source"),
                    "index" -> PLang.IntLit(0)
                )))
            )),
            PLang.FunDef("nextToken", Seq("lexer" -> PLang.UserDefinedType("Lexer")), PLang.UserDefinedType("Token"), Seq(
                PLang.Return(PLang.ObjectLit("Token", Seq(
                    "kind" -> PLang.Var("???"),
                    "line" -> PLang.Var("???"),
                    "from" -> PLang.Var("???"),
                    "to" -> PLang.Var("???")
                )))
            ))
        ))
    }
}