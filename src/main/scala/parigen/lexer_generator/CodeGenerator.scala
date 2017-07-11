package parigen.lexer_generator

import parigen.plang.PLang

object CodeGenerator {
    val thisSource = PLang.MemberAccess(PLang.This, "source")
    val thisIndex = PLang.MemberAccess(PLang.This, "index")
    val thisState = PLang.MemberAccess(PLang.This, "state")

    def fromDfa(dfa: Dfa, tokens: Map[TokenInfo.TokenID, TokenInfo.TokenType]): PLang.Module = {
        def stateName(state: Automaton.State): String = s"S_${state.toString.toUpperCase}"
        PLang.Module("lexer", Seq(
            PLang.EnumDef("TokenType", tokens.toSeq.sortBy(_._1).map {
                case (id, TokenInfo.Literal(value)) => s"literal_$id /* $value */"
                case (id, TokenInfo.Named(name)) => s"token_$name"
            } :+ "INVALID"),
            PLang.EnumDef("State", dfa.states.map(stateName) :+ "FAIL_STATE"),
            PLang.ClassDef("Lexer", Seq(
                "source" -> PLang.StringType
            ), Seq(
                PLang.VarDef("index", PLang.IntType, PLang.IntLit(0)),
                PLang.VarDef("state", PLang.UserDefinedType("State"), PLang.EnumMember("State", stateName(dfa.initialState))),
                PLang.VarDef("lastAccepting", PLang.UserDefinedType("TokenType"), PLang.EnumMember("TokenType", "INVALID")),
                PLang.VarDef("lastAcceptingIndex", PLang.IntType, PLang.IntLit(0)),
                PLang.FunDef("nextToken", Seq("lexer" -> PLang.UserDefinedType("Lexer")), PLang.UserDefinedType("Token"), Seq(
                    PLang.While(PLang.BoolLit(true), Seq(
                        PLang.Switch(thisState, dfa.states.map { state =>
                            PLang.EnumMember("State", stateName(state)) -> Seq(
                                PLang.Switch(PLang.Subscript(thisSource, thisIndex), Seq())
                            )
                        } :+ PLang.EnumMember("State", "FAIL_STATE") -> Seq(
                            PLang.Return(PLang.Instantiate("Token", Seq(
                                PLang.Var("???"),
                                PLang.Var("???"),
                                PLang.Var("???"),
                                PLang.Var("???")
                            )))
                        ))
                    ))
                ))
            )),
            PLang.ClassDef("Token", Seq(
                "kind" -> PLang.StringType,
                "line" -> PLang.IntType,
                "from" -> PLang.IntType,
                "to" -> PLang.IntType
            ), Seq())
        ))
    }
}