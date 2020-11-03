package jp.ed.nnn.parsercombinator

abstract class Combinator {
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

  /**
   * string parser
   * @param literal 文字列
   * @return
   */
  def s(literal: String): Parser[String] = string(literal)
  /**
   * > import jp.ed.nnn.parsercombinator.Combinator
   * import jp.ed.nnn.parsercombinator.Combinator
   *
   * > object Parser extends Combinator {
   *   def apply(input: String): ParseResult[String] = s("true")(input)
   * }
   * defined object Parser
   *
   * > Parser("true")
   * res0: Parser.ParseResult[String] = Success(true,)
   *
   * > Parser("hoge")
   * res1: Parser.ParseResult[String] = Failure
   */

  implicit class RichParser[T](val parser: Parser[T]) {

    /**
     * select
     *
     * @param right 選択を行うパーサー
     * @return
     */
    def |[U >: T](right: Parser[U]): Parser[U] = input => {
      parser(input) match {
        case success@Success(_, _) => success
        case Failure => right(input)
      }
    }
    /**
     * > import jp.ed.nnn.parsercombinator.Combinator
     * import jp.ed.nnn.parsercombinator.Combinator
     *
     * > object Parser extends Combinator {
     *   def apply(input: String): ParseResult[String] = (s("true")|s("false"))(input)
     * }
     * defined object Parser
     *
     * > Parser("true")
     * res0: Parser.ParseResult[String] = Success(true,)
     *
     * > Parser("false")
     * res1: Parser.ParseResult[String] = Success(false,)
     *
     * > Parser("hoge")
     * res2: Parser.ParseResult[String] = Failure
     */

    /**
     * combine
     * @param right 逐次合成を行うパーサー
     * @tparam U パーサーの結果の型
     * @return
     */
    def ~[U](right: Parser[U]) : Parser[(T, U)] = input => {
      parser(input) match {
        case Success(value1, next1) =>
          right(next1) match {
            case Success(value2, next2) =>
              Success((value1, value2), next2)
            case Failure =>
              Failure
          }
        case Failure =>
          Failure
      }
    }
    /**
     * > import jp.ed.nnn.parsercombinator.Combinator
     * import jp.ed.nnn.parsercombinator.Combinator
     *
     * > object Parser extends Combinator {
     *   def apply(input: String): ParseResult[(String, String)] = (s("[") ~ s("]"))(input)
     * }
     * defined object Parser
     *
     * > Parser("[]")
     * res0: Parser.ParseResult[(String, String)] = Success(([,]),)
     *
     * > Parser("hoge")
     * res1: Parser.ParseResult[(String, String)] = Failure
     */

    /**
     * map
     * @param function 適用する関数
     * @tparam U パーサーの結果の型
     * @return
     */
    def ^^[U](function: T => U): Parser[U] = input => {
      parser(input) match {
        case Success(value, next) => Success(function(value), next)
        case Failure => Failure
      }
    }
    /**
     * > import jp.ed.nnn.parsercombinator.Combinator
     * import jp.ed.nnn.parsercombinator.Combinator
     *
     * > object Parser extends Combinator {
     *   def apply(input: String): ParseResult[Int] = (s("1") ^^ { _.toInt })(input)
     * }
     * defined object Parser
     *
     * > Parser("1")
     * res0: Parser.ParseResult[Int] = Success(1,)
     */

    /**
     * use left
     * @param right 右側のパーサー
     * @return パーサーの結果の型
     */
    def <~(right: Parser[Any]) : Parser[T] = input => {
      parser(input) match {
        case Success(value1, next1) =>
          right(next1) match {
            case Success(value2, next2) =>
                Success(value1, next2)
            case Failure =>
                Failure
          }
          case Failure =>
            Failure
      }
    }

    /**
     * use right
     * @param right 右側のパーサー
     * @tparam U パーサーの結果の型
     * @return
     */
    def ~>[U](right: Parser[U]) : Parser[U] = input => {
      parser(input) match {
        case Success(value1, next1) =>
          right(next1) match {
            case Success(value2, next2) =>
              Success(value2, next2)
            case Failure =>
              Failure
          }
        case Failure =>
          Failure
      }
    }

    //...
  }
}
