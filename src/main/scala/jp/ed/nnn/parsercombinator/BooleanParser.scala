package jp.ed.nnn.parsercombinator

object BooleanParser extends ParserCombinator {
  def trueParser: Parser[Boolean] = map(s("true"), { _: String => true })
  def falseParser: Parser[Boolean] = map(s("false"), { _: String => false})

  def apply(input: String): ParseResult[Boolean] = select(trueParser, falseParser)(input)
}
