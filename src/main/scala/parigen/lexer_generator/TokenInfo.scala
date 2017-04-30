package parigen.lexer_generator

import parigen._
import TokenInfo._

case class TokenInfo(tokenType: TokenType, regex: Ast.Expression, id: TokenID)

object TokenInfo {
    type TokenID = Int

    sealed abstract class TokenType
    case class Named(name: String) extends TokenType
    case class Literal(value: String) extends TokenType
}