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

  def rep[T](parser: Parser[T]): Parser[List[T]] = input => {
    def repeatRec(input: String): (List[T], String) = parser(input) match {
      case Success(value, next1) =>
        val (result, next2) = repeatRec(next1)
        (value::result, next2)
      case Failure =>
        (Nil, input)
    }

    val (result, next) = repeatRec(input)
    Success(result, next)
  }

  def rep1sep[T](parser: Parser[T], sep: Parser[String]): Parser[List[T]] =
    parser ~ rep(sep ~> parser) ^^ { t => t._1 :: t._2 }

  def success[T](value: T): Parser[T] = input => Success(value, input)

  def repsep[T](parser: Parser[T], sep: Parser[String]): Parser[List[T]] =
    rep1sep(parser, sep) | success(List())
  /**
   * > import jp.ed.nnn.parsercombinator.Combinator
   * import jp.ed.nnn.parsercombinator.Combinator
   *
   * > object Parser extends Combinator {
   *   def apply(input: String): ParseResult[List[String]] = repsep(s("true"), s(","))(input)
   * }
   * |      | defined object Parser
   *
   * > Parser("true,true,true")
   * res0: Parser.ParseResult[List[String]] = Success(List(true, true, true),)
   */

  val floatingPointNumber: Parser[String] = input => {
    val r             = """^(-?\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r
    val matchIterator = r.findAllIn(input).matchData

    if(matchIterator.hasNext) {
      val next   = matchIterator.next()
      val all    = next.group(0)
      val target = next.group(1)

      Success(target, input.substring(all.length))
    } else {
      Failure
    }
  }

  val stringLiteral: Parser[String] = input =>  {
    val r             = ("^\"("+"""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*+"""+")\"").r
    val matchIterator = r.findAllIn(input).matchData

    if(matchIterator.hasNext) {
      val next   = matchIterator.next()
      val all    = next.group(0)
      val target = next.group(1)

      Success(target, input.substring(all.length))
    } else {
      Failure
    }
  }
  /**
   * > import jp.ed.nnn.parsercombinator.Combinator
   * import jp.ed.nnn.parsercombinator.Combinator
   *
   * > object Parser extends Combinator {
   *   def apply(input: String): ParseResult[String] = floatingPointNumber(input)
   * }
   * |      | defined object Parser
   *
   * > Parser("-3.14")
   * res0: Parser.ParseResult[String] = Success(-3.14,)
   *
   * > object Parser extends Combinator {
   *   def apply(input: String): ParseResult[String] = stringLiteral(input)
   * }
   * |      | defined object Parser
   *
   * > Parser("\"hoge\"")
   * res1: Parser.ParseResult[String] = Success(hoge,)
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
    /**
     * > import jp.ed.nnn.parsercombinator.Combinator
     * import jp.ed.nnn.parsercombinator.Combinator
     *
     * > object Parser extends Combinator {
     *   def apply(input: String): ParseResult[String] = (s("[") ~> s("true") <~ s("]"))(input)
     * }
     * |      | defined object Parser
     *
     * > Parser("[true]")
     * res0: Parser.ParseResult[String] = Success(true,)
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
  }
}
