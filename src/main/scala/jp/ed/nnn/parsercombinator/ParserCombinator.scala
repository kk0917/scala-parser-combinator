package jp.ed.nnn.parsercombinator

abstract class ParserCombinator {
  sealed trait ParseResult[+T]
  case class Success[+T](value: T, next: String) extends ParseResult[T]
  case object Failure extends ParseResult[Nothing]

  type Parser[+T] = String => ParseResult[T]

  def string(literal: String): Parser[String] = input => {
    if(input.startsWith(literal)) {
      Success(literal, input.substring(literal.length))
    } else {
      Failure
    }
  }
}
