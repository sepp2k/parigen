package parigen.lexer_generator

import parigen.plang.PLang._

object CodeGenerator {
    val thisSource = MemberAccess(This, "source")
    val thisIndex = MemberAccess(This, "index")
    val thisState = MemberAccess(This, "state")
    val failState = EnumMember("State", "FAIL_STATE")

    def fromDfa(dfa: Dfa, tokens: Map[TokenInfo.TokenID, TokenInfo.TokenType]): Module = {
        def stateName(state: Automaton.State): String = s"S_${state.toString.toUpperCase}"
        def enumState(state: Automaton.State): Constant = EnumMember("State", stateName(state))
        def tokenName(id: TokenInfo.TokenID) = tokens(id) match {
            case TokenInfo.Literal(value) => s"literal_$id /* $value */"
            case TokenInfo.Named(name) => s"token_$name"
        }
        def enumToken(id: TokenInfo.TokenID) = EnumMember("TokenType", tokenName(id))

        Module("lexer",
            funDef("equivalenceClassFor", IntType, "c" -> CharType)(),
            EnumDef("TokenType", tokens.keys.toSeq.sorted.map(tokenName) :+ "INVALID" :_*),
            EnumDef("State", dfa.states.map(stateName) :+ "FAIL_STATE" :_*),
            classDef("Token",
                "kind" -> UserDefinedType("TokenType"),
                "fromIndex" -> IntType,
                "toIndex" -> IntType
            )(),
            classDef("Lexer", "source" -> StringType)(
                VarDef("index", IntType, 0),
                VarDef("startIndex", IntType, 0),
                VarDef("state", UserDefinedType("State"), EnumMember("State", stateName(dfa.initialState))),
                funDef("nextToken", UserDefinedType("Token"), "lexer" -> UserDefinedType("Lexer"))(
                    VarDef("startIndex", IntType, thisIndex),
                    VarDef("lastAccepting", UserDefinedType("TokenType"), EnumMember("TokenType", "INVALID")),
                    VarDef("lastAcceptingIndex", IntType, thisIndex),
                    While(BoolLit(true),
                        Switch(thisState, None, dfa.states.map { state =>
                            Seq(EnumMember("State", stateName(state))) -> {
                                val switch = Switch(Var("equivalenceClassFor")(thisSource sub thisIndex), Some(Seq(thisState := failState)),
                                    dfa.transitions(state).groupBy(_._2).toSeq.map { case (newState, symbolsAndStates) =>
                                        val symbols = symbolsAndStates.map(_._1)
                                        symbols.map(IntLit(_)).toSeq -> Seq(thisState := enumState(newState))
                                    } : _*
                                )
                                val inc = thisIndex := thisIndex + 1
                                dfa.isAccepting(state) match {
                                    case None => Seq(switch, inc)
                                    case Some(tokenID) => Seq(
                                        Var("lastAccepting") := enumToken(tokenID),
                                        Var("lastAcceptingIndex") := thisIndex,
                                        switch,
                                        inc
                                    )
                                }
                            }
                        } :+ Seq(failState) -> Seq(
                            thisIndex := Var("lastAcceptingIndex") + 1,
                            Return(Instantiate("Token",
                                Var("lastAccepting"),
                                Var("startIndex"),
                                Var("lastAcceptingIndex")
                            ))
                        ) :_*)
                    )
                )
            )
        )
    }
}