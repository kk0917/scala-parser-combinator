package jp.ed.nnn.parsercombinator

case class PostalCode(zoneCode: String, townCode: String)

object PostalCodeParser extends ParserCombinator {
  def digit: Parser[String] = oneOf('0' to '9')

  def zoneCode: Parser[String] = map(combine(combine(digit, digit), digit), {
    t: ((String, String), String) => t._1._1 + t._1._2 + t._2
  })

  def townCode: Parser[String] = map(combine(combine(combine(digit, digit), digit), digit), {
    t: (((String, String), String), String) => t._1._1._1 + t._1._1._2 + t._1._2 + t._2
  })

  def apply(input: String): ParseResult[PostalCode] = map(combine(combine(zoneCode, s("-")), townCode), {
    t: ((String, String), String) => PostalCode(t._1._1, t._2)
  })(input)

  /**
   * > PostalCodeParser("154-0023")
   * import jp.ed.nnn.parsercombinator.PostalCodeParser
   * res0: jp.ed.nnn.parsercombinator.PostalCodeParser.ParseResult[jp.ed.nnn.parsercombinator.PostalCode] =
   *   Success(PostalCode(154,0023),)
   *
   * > PostalCodeParser("hoge")
   * res1: jp.ed.nnn.parsercombinator.PostalCodeParser.ParseResult[jp.ed.nnn.parsercombinator.PostalCode] =
   *   Failure
   */
}
