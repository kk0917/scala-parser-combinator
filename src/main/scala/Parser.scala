package jp.ed.nnn.parsercombinator

object Parser {
  /** Scala Parser
   *
   * > def parser(str: String): Int = str.toInt
   * Int
   *
   * > parser("100")
   * res0: Int = 100
   *
   * > val parser: String => Int = str => str.toInt
   * parser: String => Int = $$Lambda$1077/2124444950@30af23fd
   *
   * > parser("100")
   * res1: Int = 100
   */

  sealed trait ParseResult[+T]
  case class Success[+T](value: T, next: String) extends ParseResult[T]
  case object Failure extends ParseResult[Nothing]

  type Parser[+T] = String => ParseResult[T]

  def trueParser: Parser[Boolean] = input =>
    if (input.startsWith("true")) {
      Success(true, input.substring("true".length))
    } else {
      Failure
    }
  /** scala> trueParser
   *
   * > import jp.ed.nnn.parsercombinator.Parser._
   * import jp.ed.nnn.parsercombinator.Parser._
   *
   * > trueParser("true")
   * res0: jp.ed.nnn.parsercombinator.Parser.ParseResult[Boolean] = Success(true,)
   *
   * > trueParser("hoge")
   * res1: jp.ed.nnn.parsercombinator.Parser.ParseResult[Boolean] = Failure
   */

  def falseParser: Parser[Boolean] = input =>
    if (input.startsWith("false")) {
      Success(false, input.substring("false".length))
    } else {
      Failure
    }
}
