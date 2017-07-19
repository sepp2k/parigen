package parigen.lexer_generator

import parigen._
import TokenInfo._

case class TokenInfo(tokenType: TokenType, regex: Ast.Expression, id: TokenID) {
    override def toString = s"${tokenType.name}: $regex // ID: $id"
}

object TokenInfo {
    type TokenID = Int

    sealed abstract class TokenType {
        def name: String
    }
    case class Named(name: String) extends TokenType
    case class Literal(value: String) extends TokenType {
        def name = s""""$value""""
    }
}