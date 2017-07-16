package TODO
object lexer {
    def equivalenceClassFor(c: Char): Int = {
        if ((c == '?')) {
            return 21
        } else {
            if ((c < '?')) {
                if ((c == '(')) {
                    return 10
                } else {
                    if ((c < '(')) {
                        if ((c == '\r')) {
                            return 4
                        } else {
                            if ((c < '\r')) {
                                if ((c == '\t')) {
                                    return 1
                                } else {
                                    if ((c < '\t')) {
                                        if ((c <= '\u0008')) {
                                            return 0
                                        } else {
                                            return -1
                                        }
                                    } else {
                                        if ((c == '\n')) {
                                            return 2
                                        } else {
                                            if ((c < '\n')) {
                                                if ((c == '\n')) {
                                                    return 2
                                                } else {
                                                    return -1
                                                }
                                            } else {
                                                if (((c >= '\u000b') && (c <= '\u000c'))) {
                                                    return 3
                                                } else {
                                                    return -1
                                                }
                                            }
                                        }
                                    }
                                }
                            } else {
                                if ((c == '!')) {
                                    return 7
                                } else {
                                    if ((c < '!')) {
                                        if (((c >= '\u000e') && (c <= '\u001f'))) {
                                            return 5
                                        } else {
                                            if ((c < '\u000e')) {
                                                if (((c >= '\u000e') && (c <= '\u001f'))) {
                                                    return 5
                                                } else {
                                                    return -1
                                                }
                                            } else {
                                                if ((c == '\u0020')) {
                                                    return 6
                                                } else {
                                                    return -1
                                                }
                                            }
                                        }
                                    } else {
                                        if ((c == '\"')) {
                                            return 8
                                        } else {
                                            if ((c < '\"')) {
                                                if ((c == '\"')) {
                                                    return 8
                                                } else {
                                                    return -1
                                                }
                                            } else {
                                                if (((c >= '#') && (c <= '\''))) {
                                                    return 9
                                                } else {
                                                    return -1
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        if ((c == '.')) {
                            return 15
                        } else {
                            if ((c < '.')) {
                                if ((c == '*')) {
                                    return 12
                                } else {
                                    if ((c < '*')) {
                                        if ((c == ')')) {
                                            return 11
                                        } else {
                                            return -1
                                        }
                                    } else {
                                        if ((c == '+')) {
                                            return 13
                                        } else {
                                            if ((c < '+')) {
                                                if ((c == '+')) {
                                                    return 13
                                                } else {
                                                    return -1
                                                }
                                            } else {
                                                if (((c >= ',') && (c <= '-'))) {
                                                    return 14
                                                } else {
                                                    return -1
                                                }
                                            }
                                        }
                                    }
                                }
                            } else {
                                if ((c == ':')) {
                                    return 18
                                } else {
                                    if ((c < ':')) {
                                        if ((c == '/')) {
                                            return 16
                                        } else {
                                            if ((c < '/')) {
                                                if ((c == '/')) {
                                                    return 16
                                                } else {
                                                    return -1
                                                }
                                            } else {
                                                if (((c >= '0') && (c <= '9'))) {
                                                    return 17
                                                } else {
                                                    return -1
                                                }
                                            }
                                        }
                                    } else {
                                        if ((c == ';')) {
                                            return 19
                                        } else {
                                            if ((c < ';')) {
                                                if ((c == ';')) {
                                                    return 19
                                                } else {
                                                    return -1
                                                }
                                            } else {
                                                if (((c >= '<') && (c <= '>'))) {
                                                    return 20
                                                } else {
                                                    return -1
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                if (((c >= 'f') && (c <= 'j'))) {
                    return 32
                } else {
                    if ((c < 'f')) {
                        if ((c == ']')) {
                            return 26
                        } else {
                            if ((c < ']')) {
                                if (((c >= 'A') && (c <= 'Z'))) {
                                    return 23
                                } else {
                                    if ((c < 'A')) {
                                        if ((c == '@')) {
                                            return 22
                                        } else {
                                            return -1
                                        }
                                    } else {
                                        if ((c == '[')) {
                                            return 24
                                        } else {
                                            if ((c < '[')) {
                                                if ((c == '[')) {
                                                    return 24
                                                } else {
                                                    return -1
                                                }
                                            } else {
                                                if ((c == '\\')) {
                                                    return 25
                                                } else {
                                                    return -1
                                                }
                                            }
                                        }
                                    }
                                }
                            } else {
                                if ((c == '`')) {
                                    return 29
                                } else {
                                    if ((c < '`')) {
                                        if ((c == '^')) {
                                            return 27
                                        } else {
                                            if ((c < '^')) {
                                                if ((c == '^')) {
                                                    return 27
                                                } else {
                                                    return -1
                                                }
                                            } else {
                                                if ((c == '_')) {
                                                    return 28
                                                } else {
                                                    return -1
                                                }
                                            }
                                        }
                                    } else {
                                        if (((c >= 'a') && (c <= 'd'))) {
                                            return 30
                                        } else {
                                            if ((c < 'a')) {
                                                if (((c >= 'a') && (c <= 'd'))) {
                                                    return 30
                                                } else {
                                                    return -1
                                                }
                                            } else {
                                                if ((c == 'e')) {
                                                    return 31
                                                } else {
                                                    return -1
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        if (((c >= 'p') && (c <= 's'))) {
                            return 37
                        } else {
                            if ((c < 'p')) {
                                if (((c >= 'l') && (c <= 'm'))) {
                                    return 34
                                } else {
                                    if ((c < 'l')) {
                                        if ((c == 'k')) {
                                            return 33
                                        } else {
                                            return -1
                                        }
                                    } else {
                                        if ((c == 'n')) {
                                            return 35
                                        } else {
                                            if ((c < 'n')) {
                                                if ((c == 'n')) {
                                                    return 35
                                                } else {
                                                    return -1
                                                }
                                            } else {
                                                if ((c == 'o')) {
                                                    return 36
                                                } else {
                                                    return -1
                                                }
                                            }
                                        }
                                    }
                                }
                            } else {
                                if ((c == '{')) {
                                    return 40
                                } else {
                                    if ((c < '{')) {
                                        if ((c == 't')) {
                                            return 38
                                        } else {
                                            if ((c < 't')) {
                                                if ((c == 't')) {
                                                    return 38
                                                } else {
                                                    return -1
                                                }
                                            } else {
                                                if (((c >= 'u') && (c <= 'z'))) {
                                                    return 39
                                                } else {
                                                    return -1
                                                }
                                            }
                                        }
                                    } else {
                                        if ((c == '|')) {
                                            return 41
                                        } else {
                                            if ((c < '|')) {
                                                if ((c == '|')) {
                                                    return 41
                                                } else {
                                                    return -1
                                                }
                                            } else {
                                                if ((c >= '}')) {
                                                    return 42
                                                } else {
                                                    return -1
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        sys.error("Missing return statement")
    }
    sealed abstract class TokenType
    case object TokenType_literal_0 /* token */ extends TokenType
    case object TokenType_literal_1 /* : */ extends TokenType
    case object TokenType_literal_2 /* ; */ extends TokenType
    case object TokenType_literal_3 /* | */ extends TokenType
    case object TokenType_literal_4 /* * */ extends TokenType
    case object TokenType_literal_5 /* + */ extends TokenType
    case object TokenType_literal_6 /* ? */ extends TokenType
    case object TokenType_literal_7 /* . */ extends TokenType
    case object TokenType_literal_8 /* ( */ extends TokenType
    case object TokenType_literal_9 /* ) */ extends TokenType
    case object TokenType_token_CHARACTER_CLASS extends TokenType
    case object TokenType_token_ID extends TokenType
    case object TokenType_token_STRING_LIT extends TokenType
    case object TokenType_token_WS extends TokenType
    case object TokenType_INVALID extends TokenType
    sealed abstract class State
    case object State_S_NPE4NQ extends State
    case object State_S_YBK5H7 extends State
    case object State_S_HEILRM extends State
    case object State_S_NRTSGI extends State
    case object State_S_WDT2VM extends State
    case object State_S_JW2D1W extends State
    case object State_S_7P5T14 extends State
    case object State_S_SSSFTT extends State
    case object State_S_CA4HWR extends State
    case object State_S_RMOWA5 extends State
    case object State_S_M3IRN9 extends State
    case object State_S_1KE3UP extends State
    case object State_S_X4P0T3 extends State
    case object State_S_CP6BC8 extends State
    case object State_S_QLN946 extends State
    case object State_S_F6Y229 extends State
    case object State_S_XX32OV extends State
    case object State_S_L6G5QK extends State
    case object State_S_VFCPS2 extends State
    case object State_S_WWY9YU extends State
    case object State_S_70E35C extends State
    case object State_S_CE7EJT extends State
    case object State_S_98MJLN extends State
    case object State_S_K0T3E1 extends State
    case object State_S_VEVA45 extends State
    case object State_FAIL_STATE extends State
    case class Token(kind: TokenType, fromIndex: Int, toIndex: Int, value: String) {
    }
    case class Lexer(source: String) {
        var index: Int = 0
        var startIndex: Int = 0
        var state: State = State_S_SSSFTT
        def hasNext(): Boolean = {
            return (this.index < this.source.length)
            sys.error("Missing return statement")
        }
        def nextToken(): Token = {
            var startIndex: Int = this.index
            var lastAccepting: TokenType = TokenType_INVALID
            var lastAcceptingIndex: Int = this.index
            while(index < source.length) {
                this.state match {
                case State_S_NPE4NQ =>
                    lastAccepting = TokenType_token_WS
                    lastAcceptingIndex = (this.index - 1)
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case 2 =>
                    case 4 =>
                    case 1 =>
                    case 6 =>
                        this.state = State_S_VFCPS2
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_YBK5H7 =>
                    lastAccepting = TokenType_token_ID
                    lastAcceptingIndex = (this.index - 1)
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case 37 =>
                    case 28 =>
                    case 38 =>
                    case 33 =>
                    case 32 =>
                    case 34 =>
                    case 17 =>
                    case 39 =>
                    case 35 =>
                    case 31 =>
                    case 23 =>
                    case 36 =>
                    case 30 =>
                        this.state = State_S_HEILRM
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_HEILRM =>
                    lastAccepting = TokenType_token_ID
                    lastAcceptingIndex = (this.index - 1)
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case 37 =>
                    case 28 =>
                    case 38 =>
                    case 33 =>
                    case 32 =>
                    case 34 =>
                    case 17 =>
                    case 39 =>
                    case 35 =>
                    case 31 =>
                    case 23 =>
                    case 36 =>
                    case 30 =>
                        this.state = State_S_HEILRM
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_NRTSGI =>
                    lastAccepting = TokenType_literal_3 /* | */
                    lastAcceptingIndex = (this.index - 1)
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_WDT2VM =>
                    lastAccepting = TokenType_literal_1 /* : */
                    lastAcceptingIndex = (this.index - 1)
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_JW2D1W =>
                    lastAccepting = TokenType_token_ID
                    lastAcceptingIndex = (this.index - 1)
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case 37 =>
                    case 28 =>
                    case 38 =>
                    case 33 =>
                    case 32 =>
                    case 34 =>
                    case 17 =>
                    case 39 =>
                    case 31 =>
                    case 23 =>
                    case 36 =>
                    case 30 =>
                        this.state = State_S_HEILRM
                    case 35 =>
                        this.state = State_S_CE7EJT
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_7P5T14 =>
                    lastAccepting = TokenType_literal_8 /* ( */
                    lastAcceptingIndex = (this.index - 1)
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_SSSFTT =>
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case 19 =>
                        this.state = State_S_K0T3E1
                    case 24 =>
                        this.state = State_S_QLN946
                    case 41 =>
                        this.state = State_S_NRTSGI
                    case 10 =>
                        this.state = State_S_7P5T14
                    case 8 =>
                        this.state = State_S_F6Y229
                    case 18 =>
                        this.state = State_S_WDT2VM
                    case 21 =>
                        this.state = State_S_M3IRN9
                    case 1 =>
                    case 6 =>
                    case 2 =>
                    case 4 =>
                        this.state = State_S_NPE4NQ
                    case 12 =>
                        this.state = State_S_70E35C
                    case 37 =>
                    case 28 =>
                    case 33 =>
                    case 32 =>
                    case 34 =>
                    case 39 =>
                    case 35 =>
                    case 31 =>
                    case 23 =>
                    case 36 =>
                    case 30 =>
                        this.state = State_S_YBK5H7
                    case 38 =>
                        this.state = State_S_X4P0T3
                    case 15 =>
                        this.state = State_S_98MJLN
                    case 13 =>
                        this.state = State_S_XX32OV
                    case 11 =>
                        this.state = State_S_WWY9YU
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_CA4HWR =>
                    lastAccepting = TokenType_token_ID
                    lastAcceptingIndex = (this.index - 1)
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case 37 =>
                    case 28 =>
                    case 38 =>
                    case 33 =>
                    case 32 =>
                    case 34 =>
                    case 17 =>
                    case 39 =>
                    case 35 =>
                    case 23 =>
                    case 36 =>
                    case 30 =>
                        this.state = State_S_HEILRM
                    case 31 =>
                        this.state = State_S_JW2D1W
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_RMOWA5 =>
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case 0 =>
                    case 5 =>
                    case 10 =>
                    case 42 =>
                    case 24 =>
                    case 37 =>
                    case 25 =>
                    case 14 =>
                    case 20 =>
                    case 29 =>
                    case 1 =>
                    case 6 =>
                    case 28 =>
                    case 38 =>
                    case 21 =>
                    case 33 =>
                    case 9 =>
                    case 13 =>
                    case 41 =>
                    case 2 =>
                    case 32 =>
                    case 34 =>
                    case 17 =>
                    case 22 =>
                    case 27 =>
                    case 12 =>
                    case 7 =>
                    case 39 =>
                    case 3 =>
                    case 35 =>
                    case 18 =>
                    case 16 =>
                    case 31 =>
                    case 11 =>
                    case 40 =>
                    case 26 =>
                    case 23 =>
                    case 8 =>
                    case 36 =>
                    case 30 =>
                    case 19 =>
                    case 4 =>
                    case 15 =>
                        this.state = State_S_QLN946
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_M3IRN9 =>
                    lastAccepting = TokenType_literal_6 /* ? */
                    lastAcceptingIndex = (this.index - 1)
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_1KE3UP =>
                    lastAccepting = TokenType_token_ID
                    lastAcceptingIndex = (this.index - 1)
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case 37 =>
                    case 28 =>
                    case 38 =>
                    case 32 =>
                    case 34 =>
                    case 17 =>
                    case 39 =>
                    case 35 =>
                    case 31 =>
                    case 23 =>
                    case 36 =>
                    case 30 =>
                        this.state = State_S_HEILRM
                    case 33 =>
                        this.state = State_S_CA4HWR
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_X4P0T3 =>
                    lastAccepting = TokenType_token_ID
                    lastAcceptingIndex = (this.index - 1)
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case 37 =>
                    case 28 =>
                    case 38 =>
                    case 33 =>
                    case 32 =>
                    case 34 =>
                    case 17 =>
                    case 39 =>
                    case 35 =>
                    case 31 =>
                    case 23 =>
                    case 30 =>
                        this.state = State_S_HEILRM
                    case 36 =>
                        this.state = State_S_1KE3UP
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_CP6BC8 =>
                    lastAccepting = TokenType_token_CHARACTER_CLASS
                    lastAcceptingIndex = (this.index - 1)
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_QLN946 =>
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case 26 =>
                        this.state = State_S_CP6BC8
                    case 0 =>
                    case 5 =>
                    case 10 =>
                    case 42 =>
                    case 24 =>
                    case 37 =>
                    case 14 =>
                    case 20 =>
                    case 29 =>
                    case 1 =>
                    case 6 =>
                    case 28 =>
                    case 38 =>
                    case 21 =>
                    case 33 =>
                    case 9 =>
                    case 13 =>
                    case 41 =>
                    case 2 =>
                    case 32 =>
                    case 34 =>
                    case 17 =>
                    case 22 =>
                    case 27 =>
                    case 12 =>
                    case 7 =>
                    case 39 =>
                    case 3 =>
                    case 35 =>
                    case 18 =>
                    case 16 =>
                    case 31 =>
                    case 11 =>
                    case 40 =>
                    case 23 =>
                    case 8 =>
                    case 36 =>
                    case 30 =>
                    case 19 =>
                    case 4 =>
                    case 15 =>
                        this.state = State_S_QLN946
                    case 25 =>
                        this.state = State_S_RMOWA5
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_F6Y229 =>
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case 0 =>
                    case 5 =>
                    case 10 =>
                    case 42 =>
                    case 24 =>
                    case 37 =>
                    case 14 =>
                    case 20 =>
                    case 29 =>
                    case 1 =>
                    case 6 =>
                    case 28 =>
                    case 38 =>
                    case 21 =>
                    case 33 =>
                    case 9 =>
                    case 13 =>
                    case 41 =>
                    case 2 =>
                    case 32 =>
                    case 34 =>
                    case 17 =>
                    case 22 =>
                    case 27 =>
                    case 12 =>
                    case 7 =>
                    case 39 =>
                    case 3 =>
                    case 35 =>
                    case 18 =>
                    case 16 =>
                    case 31 =>
                    case 11 =>
                    case 40 =>
                    case 26 =>
                    case 23 =>
                    case 36 =>
                    case 30 =>
                    case 19 =>
                    case 4 =>
                    case 15 =>
                        this.state = State_S_F6Y229
                    case 8 =>
                        this.state = State_S_VEVA45
                    case 25 =>
                        this.state = State_S_L6G5QK
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_XX32OV =>
                    lastAccepting = TokenType_literal_5 /* + */
                    lastAcceptingIndex = (this.index - 1)
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_L6G5QK =>
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case 0 =>
                    case 5 =>
                    case 10 =>
                    case 42 =>
                    case 24 =>
                    case 37 =>
                    case 25 =>
                    case 14 =>
                    case 20 =>
                    case 29 =>
                    case 1 =>
                    case 6 =>
                    case 28 =>
                    case 38 =>
                    case 21 =>
                    case 33 =>
                    case 9 =>
                    case 13 =>
                    case 41 =>
                    case 2 =>
                    case 32 =>
                    case 34 =>
                    case 17 =>
                    case 22 =>
                    case 27 =>
                    case 12 =>
                    case 7 =>
                    case 39 =>
                    case 3 =>
                    case 35 =>
                    case 18 =>
                    case 16 =>
                    case 31 =>
                    case 11 =>
                    case 40 =>
                    case 26 =>
                    case 23 =>
                    case 8 =>
                    case 36 =>
                    case 30 =>
                    case 19 =>
                    case 4 =>
                    case 15 =>
                        this.state = State_S_F6Y229
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_VFCPS2 =>
                    lastAccepting = TokenType_token_WS
                    lastAcceptingIndex = (this.index - 1)
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case 2 =>
                    case 4 =>
                    case 1 =>
                    case 6 =>
                        this.state = State_S_VFCPS2
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_WWY9YU =>
                    lastAccepting = TokenType_literal_9 /* ) */
                    lastAcceptingIndex = (this.index - 1)
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_70E35C =>
                    lastAccepting = TokenType_literal_4 /* * */
                    lastAcceptingIndex = (this.index - 1)
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_CE7EJT =>
                    lastAccepting = TokenType_literal_0 /* token */
                    lastAcceptingIndex = (this.index - 1)
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case 37 =>
                    case 28 =>
                    case 38 =>
                    case 33 =>
                    case 32 =>
                    case 34 =>
                    case 17 =>
                    case 39 =>
                    case 35 =>
                    case 31 =>
                    case 23 =>
                    case 36 =>
                    case 30 =>
                        this.state = State_S_HEILRM
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_98MJLN =>
                    lastAccepting = TokenType_literal_7 /* . */
                    lastAcceptingIndex = (this.index - 1)
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_K0T3E1 =>
                    lastAccepting = TokenType_literal_2 /* ; */
                    lastAcceptingIndex = (this.index - 1)
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_S_VEVA45 =>
                    lastAccepting = TokenType_token_STRING_LIT
                    lastAcceptingIndex = (this.index - 1)
                    equivalenceClassFor(this.source.charAt(this.index)) match {
                    case _ =>
                        this.state = State_FAIL_STATE
                    }
                    this.index = (this.index + 1)
                case State_FAIL_STATE =>
                    this.index = (lastAcceptingIndex + 1)
                    this.state = State_S_SSSFTT
                    return Token(lastAccepting, startIndex, lastAcceptingIndex, this.source.substring(startIndex, (lastAcceptingIndex + 1)))
                }
            }
            return Token(lastAccepting, startIndex, lastAcceptingIndex, this.source.substring(startIndex, (lastAcceptingIndex + 1)))
        }
    }

    def main(args: Array[String]) = {
        val lexer = Lexer("""
grammar: rule*;
rule: "token"? ID ":" expression ";";
expression: sequence ("|" sequence)*;
sequence: quantifiedExpression+;
quantifiedExpression: primaryExpression quantifier?;
quantifier: "*" | "+" | "?";
primaryExpression:
    ID |
    STRING_LIT |
    CHARACTER_CLASS |
    "." |
    "(" expression ")"
    ;
token CHARACTER_CLASS: "[" ("\\" . | [^\]\\])* "]";
token ID: [a-zA-Z_][a-zA-Z_0-9]*;
token STRING_LIT: "\"" ("\\" . | [^"\\])* "\"";
token WS: [ \t\r\n]+;
        """)
        while (lexer.hasNext) {
            println(lexer.nextToken)
        }
    }
}
