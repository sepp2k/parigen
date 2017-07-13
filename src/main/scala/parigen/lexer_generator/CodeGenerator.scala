package parigen.lexer_generator

import parigen.plang.PLang
import parigen.plang.PLang.makeLiteral

object CodeGenerator {
    val thisSource = PLang.MemberAccess(PLang.This, "source")
    val thisIndex = PLang.MemberAccess(PLang.This, "index")
    val thisState = PLang.MemberAccess(PLang.This, "state")
    val thisLastAccepting = PLang.MemberAccess(PLang.This, "lastAccepting")
    val thisLastAcceptingIndex = PLang.MemberAccess(PLang.This, "lastAcceptingIndex")
    val failState = PLang.EnumMember("State", "FAIL_STATE")

    def fromDfa(dfa: Dfa, tokens: Map[TokenInfo.TokenID, TokenInfo.TokenType]): PLang.Module = {
        def stateName(state: Automaton.State): String = s"S_${state.toString.toUpperCase}"
        def enumState(state: Automaton.State): PLang.Constant = PLang.EnumMember("State", stateName(state))
        def tokenName(id: TokenInfo.TokenID) = tokens(id) match {
            case TokenInfo.Literal(value) => s"literal_$id /* $value */"
            case TokenInfo.Named(name) => s"token_$name"
        }
        def enumToken(id: TokenInfo.TokenID) = PLang.EnumMember("TokenType", tokenName(id))

        PLang.Module("lexer",
            PLang.funDef("equivalenceClassFor", PLang.IntType, "c" -> PLang.CharType)(),
            PLang.EnumDef("TokenType", tokens.keys.toSeq.sorted.map(tokenName) :+ "INVALID" :_*),
            PLang.EnumDef("State", dfa.states.map(stateName) :+ "FAIL_STATE" :_*),
            PLang.classDef("Token",
                "kind" -> PLang.UserDefinedType("TokenType"),
                "line" -> PLang.IntType,
                "from" -> PLang.IntType,
                "to" -> PLang.IntType
            )(),
            PLang.classDef("Lexer", "source" -> PLang.StringType)(
                PLang.VarDef("index", PLang.IntType, 0),
                PLang.VarDef("state", PLang.UserDefinedType("State"), PLang.EnumMember("State", stateName(dfa.initialState))),
                PLang.VarDef("lastAccepting", PLang.UserDefinedType("TokenType"), PLang.EnumMember("TokenType", "INVALID")),
                PLang.VarDef("lastAcceptingIndex", PLang.IntType, 0),
                PLang.funDef("nextToken", PLang.UserDefinedType("Token"), "lexer" -> PLang.UserDefinedType("Lexer"))(
                    PLang.While(PLang.BoolLit(true),
                        PLang.Switch(thisState, None, dfa.states.map { state =>
                            PLang.EnumMember("State", stateName(state)) -> {
                                val switch = PLang.Switch(PLang.Var("equivalenceClassFor")(thisSource sub thisIndex), Some(Seq(thisState := failState)),
                                    dfa.transitions(state).toSeq.map { case (symbol, newState) =>
                                        PLang.IntLit(symbol) -> Seq(thisState := enumState(newState))
                                    } : _*
                                )
                                val inc = thisIndex := thisIndex + 1
                                dfa.isAccepting(state) match {
                                    case None => Seq(switch, inc)
                                    case Some(tokenID) => Seq(
                                        thisLastAccepting := enumToken(tokenID),
                                        thisLastAcceptingIndex := thisIndex,
                                        switch,
                                        inc
                                    )
                                }
                            }
                        } :+ failState -> Seq(
                            PLang.Return(PLang.Instantiate("Token",
                                PLang.Var("???"),
                                PLang.Var("???"),
                                PLang.Var("???"),
                                PLang.Var("???")
                            ))
                        ) :_*)
                    )
                )
            )
        )
    }
}