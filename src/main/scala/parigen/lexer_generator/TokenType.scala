package parigen.lexer_generator

sealed abstract class TokenType
case class Named(name: String) extends TokenType
case class Literal(value: String) extends TokenType