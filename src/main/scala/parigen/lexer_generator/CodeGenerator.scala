package parigen.lexer_generator

import parigen.plang.PLang._

object CodeGenerator {
    val thisSource = MemberAccess(This, "source")
    val thisIndex = MemberAccess(This, "index")
    val thisState = MemberAccess(This, "state")
    val failState = EnumMember("State", "FAIL_STATE")

    // Turn the equivalence classes of the alphabet into a balanced binary tree of if-statements
    def equivalenceClasses(alphabet: Vector[(Automaton.EquivalenceClass, Automaton.Symbol)], low: Int, high: Int): Statement = {
        val mid = low + (high - low) / 2
        val ((lowChar, highChar), id) = alphabet(mid)
        val elseCase: Statement = if (low >= high) Return(-1) else {
            if (lowChar == '\u0000') equivalenceClasses(alphabet, mid + 1, high)
            else if (highChar == '\uffff') equivalenceClasses(alphabet, low, mid)
            else IfThenElse(Var("c") < lowChar, Seq(equivalenceClasses(alphabet, low, mid)), Seq(equivalenceClasses(alphabet, mid + 1, high)))
        }
        val cond =
            if (lowChar == highChar) Var("c") === lowChar
            else if (lowChar == '\u0000') Var("c") <= highChar
            else if (highChar == '\uffff') Var("c") >= lowChar
            else Var("c") >= lowChar &&& Var("c") <= highChar
        IfThenElse(cond, Seq(Return(id)), Seq(elseCase))
    }

    def equivalenceClasses(alphabet: Automaton.IndexedAlphabet): Statement = {
        equivalenceClasses(alphabet.toVector, 0, alphabet.size - 1)
    }

    def fromDfa(dfa: Dfa, tokens: Map[TokenInfo.TokenID, TokenInfo.TokenType]): Module = {
        def stateName(state: Automaton.State): String = s"S_${state.toString.toUpperCase}"
        def enumState(state: Automaton.State): Constant = EnumMember("State", stateName(state))
        def tokenName(id: TokenInfo.TokenID) = tokens(id) match {
            case TokenInfo.Literal(value) => s"literal_$id /* $value */"
            case TokenInfo.Named(name) => s"token_$name"
        }
        def enumToken(id: TokenInfo.TokenID) = EnumMember("TokenType", tokenName(id))

        Module("lexer",
            funDef("equivalenceClassFor", IntType, "c" -> CharType)(equivalenceClasses(dfa.alphabet)),
            EnumDef("TokenType", tokens.keys.toSeq.sorted.map(tokenName) :+ "INVALID" :_*),
            EnumDef("State", dfa.states.map(stateName) :+ "FAIL_STATE" :_*),
            classDef("Token",
                "kind" -> UserDefinedType("TokenType"),
                "fromIndex" -> IntType,
                "toIndex" -> IntType,
                "value" -> StringType
            )(),
            classDef("Lexer", "source" -> StringType)(
                VarDef("index", IntType, 0),
                VarDef("startIndex", IntType, 0),
                VarDef("state", UserDefinedType("State"), EnumMember("State", stateName(dfa.initialState))),
                funDef("hasNext", BoolType)(Return(thisIndex < MemberAccess(thisSource, "length"))),
                funDef("nextToken", UserDefinedType("Token"))(
                    VarDef("startIndex", IntType, thisIndex),
                    VarDef("lastAccepting", UserDefinedType("TokenType"), EnumMember("TokenType", "INVALID")),
                    VarDef("lastAcceptingIndex", IntType, thisIndex),
                    While(BoolLit(true),
                        Switch(thisState, None, dfa.states.map { state =>
                            Seq(EnumMember("State", stateName(state))) -> {
                                val switch = Switch(Var("equivalenceClassFor")(thisSource charAt thisIndex), Some(Seq(thisState := failState)),
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
                                        Var("lastAcceptingIndex") := thisIndex - 1,
                                        switch,
                                        inc
                                    )
                                }
                            }
                        } :+ Seq(failState) -> Seq(
                            thisIndex := Var("lastAcceptingIndex") + 1,
                            thisState := enumState(dfa.initialState),
                            Return(Instantiate("Token",
                                Var("lastAccepting"),
                                Var("startIndex"),
                                Var("lastAcceptingIndex"),
                                thisSource.member("substring")(Var("startIndex"), Var("lastAcceptingIndex") + 1)
                            ))
                        ) :_*)
                    )
                )
            )
        )
    }
}