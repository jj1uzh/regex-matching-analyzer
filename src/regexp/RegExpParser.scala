package matching.regexp

import scala.util.parsing.combinator._

object RegExpParser extends RegexParsers {
  def apply(s: String): RegExp[Char] = {
    parseAll(expAnchor, s) match {
      case Success(r,_) => r
      case NoSuccess(msg,next) => throw new Exception(s"${msg} at ${next.pos.line}:${next.pos.column}")
    }
  }

  val specialChars = """∅ε.,|*+?^$(){}\[\]\\"""
  val specialCharsInCharClass = """\^\[\]\-\\"""
  val chars = s"""[^\\s${specialChars}]""".r
  val escs = s"""\\\\[${specialChars}]""".r
  val metas = """\\[stnwd]""".r
  val charsInCharClass = s"""[^\\s${specialCharsInCharClass}]""".r
  val escsInCharClass = s"""\\\\[${specialCharsInCharClass}]""".r

  def toChar(s: String): Char = {
    if (s.head == '\\') s.last else s.head
  }

  def expAnchor: Parser[RegExp[Char]] = opt("^") ~ rep1sep(term,"|") ~ opt("$") ^^ { case start ~ ts ~ end =>
    RegExp.optConcatExp(
      RegExp.optConcatExp(
        if (start.isDefined) EpsExp() else StarExp(DotExp(),false),
        ts.reduceLeft(AltExp(_,_))
      ),
      if (end.isDefined) EpsExp() else StarExp(DotExp(),true)
    )
  }

  def exp: Parser[RegExp[Char]] = rep1sep(term,"|") ^^ {_.reduceLeft(AltExp(_,_))}
  def term: Parser[RegExp[Char]] = rep1(quantifiedFactor) ^^ {_.reduceLeft(ConcatExp(_,_))}
  def quantifiedFactor: Parser[RegExp[Char]] = factor ~ opt(quantifier ~ opt("?")) ^^ {
    case f ~ Some(q ~ opt) =>
      val greedy = opt.isEmpty
      q match {
        case Right(s) => s match {
          case "*" => StarExp(f,greedy)
          case "+" => PlusExp(f,greedy)
          case "?" => OptionExp(f,greedy)
        }
        case Left((min,max)) => RepeatExp(f,min,max,greedy)
      }
    case f ~ None => f
  }
  def quantifier: Parser[Either[(Option[Int],Option[Int]),String]] = symbol | repeat
  def symbol: Parser[Right[Nothing,String]] = "[*+?]".r ^^ {Right(_)}
  def repeat: Parser[Left[(Option[Int],Option[Int]),Nothing]] = (minmax | exact) ^^ {Left(_)}
  def minmax: Parser[(Option[Int],Option[Int])] = ("{" ~> opt(posNum) <~ ",") ~ opt(posNum) <~ "}" ^^ {
    case min ~ max => (min, max)
  }
  def exact: Parser[(Option[Int],Option[Int])] = "{" ~> posNum <~ "}" ^^ {case num => (Some(num), Some(num))}
  def posNum: Parser[Int] = """[1-9]\d*""".r ^^ {_.toInt}
  def factor: Parser[RegExp[Char]] = elem | empty | eps | dot | group | charClassNeg | charClass
  def elem: Parser[RegExp[Char]] = charEsc | meta
  def charEsc: Parser[ElemExp[Char]] = (chars | escs) ^^ {s => ElemExp(toChar(s))}
  def meta: Parser[MetaCharExp] = metas ^^ {s => MetaCharExp(s.last)}
  def empty: Parser[EmptyExp[Char]] = "∅" ^^ {_ => EmptyExp()}
  def eps: Parser[EpsExp[Char]] = "ε" ^^ {_ => EpsExp()}
  def dot: Parser[DotExp[Char]] = "." ^^ {_ => DotExp()}
  def group: Parser[RegExp[Char]] = "(" ~> exp <~ ")"
  def charClass: Parser[CharClassExp] = "[" ~> charClassElems <~ "]" ^^ {CharClassExp(_,true)}
  def charClassNeg: Parser[CharClassExp] = "[^" ~> charClassElems <~ "]" ^^ {CharClassExp(_,false)}
  def charClassElems: Parser[Seq[CharClassElem]] = rep1(range | singleChar)
  def singleChar: Parser[SingleCharExp] = charClassElem ^^ {SingleCharExp}
  def range: Parser[RangeExp] = (charClassElem <~ "-") ~ charClassElem ^^ {
    case start ~ end => RangeExp(start, end)
  }
  def charClassElem: Parser[Char] = (charsInCharClass | escsInCharClass) ^^ {toChar}
}
