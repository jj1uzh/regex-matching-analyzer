package matching.regexp

import matching.regexp.KleeneRegex.ExtendedChar.CloseParen
import matching.regexp.KleeneRegex.ExtendedChar.OpenParen
import matching.regexp.KleeneRegex.ExtendedChar.Reference
import matching.regexp.KleeneRegex.ExtendedChar.WrappedChar
import matching.regexp.KleeneRegex._
import matching.tool.Visualizer

sealed trait KleeneRegex[+A] {

  override def toString(): String = this match {
    case Empty          => "∅"
    case Epsilon        => "ε"
    case Atom(a)        => a.toString
    case Concat(r1, r2) => s"$r1$r2"
    case Union(r1, r2)  => s"($r1|$r2)"
    case Star(r)        => s"($r)*"
  }

  /** Whether the language of this regex contains epsilon. */
  lazy val nullable: Boolean = this match {
    case Empty | Atom(_)   => false
    case Epsilon | Star(_) => true
    case Concat(r1, r2)    => r1.nullable && r2.nullable
    case Union(r1, r2)     => r1.nullable || r2.nullable
  }

  private def toNull[B >: A]: KleeneRegex[B] =
    if (nullable) Epsilon else Empty

  protected def simple[B >: A]: KleeneRegex[B] = this match {
    case r @ (Empty | Epsilon | Atom(_)) => r
    case Concat(Epsilon, r)              => r
    case Concat(r, Epsilon)              => r
    case Concat(Empty, _)                => Empty
    case Concat(_, Empty)                => Empty
    case Union(Empty, r)                 => r
    case Union(r, Empty)                 => r
    case Star(Epsilon)                   => Epsilon
    case Star(Empty)                     => Empty
    case other                           => other
  }

  def deriv[B >: A](a: B): KleeneRegex[B] = this match {
    case Empty | Epsilon => Empty
    case Atom(b)         => if (a == b) Epsilon else Empty
    case Concat(r1, r2) =>
      Union.simple(Concat.simple(r1.deriv(a), r2), Concat.simple(r1.toNull, r2.deriv(a)))
    case Union(r1, r2) => Union.simple(r1.deriv(a), r2.deriv(a))
    case s @ Star(r1)  => Concat.simple(r1.deriv(a), s)
  }

  def alphabets[B >: A]: Set[B] = this match {
    case Empty | Epsilon => Set()
    case Atom(a)         => Set(a)
    case Concat(r1, r2)  => r1.alphabets ++ r2.alphabets
    case Union(r1, r2)   => r1.alphabets ++ r2.alphabets
    case Star(r)         => r.alphabets
  }
}

object KleeneRegex {

  case object Empty extends KleeneRegex[Nothing]
  case object Epsilon extends KleeneRegex[Nothing]
  case class Atom[A](a: A) extends KleeneRegex[A]
  case class Concat[A](r1: KleeneRegex[A], r2: KleeneRegex[A]) extends KleeneRegex[A]
  case class Union[A](r1: KleeneRegex[A], r2: KleeneRegex[A]) extends KleeneRegex[A]
  case class Star[A](r: KleeneRegex[A]) extends KleeneRegex[A]

  object Concat {
    def simple[A](r1: KleeneRegex[A], r2: => KleeneRegex[A]): KleeneRegex[A] =
      r1 match {
        case Empty              => Empty
        case Epsilon            => r2
        case Concat(r1_1, r1_2) => Concat(r1_1, Concat(r1_2, r2))
        case r1 =>
          r2 match {
            case Empty   => Empty
            case Epsilon => r1
            case r2      => Concat(r1, r2)
          }
      }
  }

  object Union {
    def simple[A](r1: KleeneRegex[A], r2: => KleeneRegex[A]): KleeneRegex[A] =
      r1 match {
        case Empty             => r2
        case Union(r1_1, r1_2) => Union(r1_1, Union(r1_2, r2))
        case r1 =>
          r2 match {
            case Empty          => r1
            case r2 if r1 == r2 => r1
            case r2             => Union(r1, r2)
          }
      }
  }

  object Star {
    def simple[A](r: KleeneRegex[A]): KleeneRegex[A] =
      r match {
        case Epsilon | Empty => Epsilon
        case s @ Star(_)     => s
        case r               => Star(r)
      }
  }

  sealed trait ExtendedChar extends Product with Serializable {
    override def toString(): String = this match {
      case OpenParen(id)  => s"(${Visualizer.subscript(id)}"
      case CloseParen(id) => s")${Visualizer.subscript(id)}"
      case Reference(id)  => s"\\$id"
      case WrappedChar(c) => c.toString
    }
  }
  object ExtendedChar {
    case class OpenParen(id: Int) extends ExtendedChar
    case class CloseParen(id: Int) extends ExtendedChar
    case class Reference(id: Int) extends ExtendedChar
    case class WrappedChar(c: Char) extends ExtendedChar
  }
  import ExtendedChar._

  def fromRegExp(r: RegExp[Char]): (KleeneRegex[ExtendedChar], Set[Int]) = r match {
    case EmptyExp() => (Empty, Set())
    case EpsExp()   => (Epsilon, Set())
    case ElemExp(a) => (Atom(WrappedChar(a)), Set())
    case ConcatExp(r1, r2) =>
      val (r1r, ids1) = fromRegExp(r1)
      val (r2r, ids2) = fromRegExp(r2)
      (Concat(r1r, r2r), ids1 ++ ids2)
    case AltExp(r1, r2) =>
      val (r1r, ids1) = fromRegExp(r1)
      val (r2r, ids2) = fromRegExp(r2)
      (Union(r1r, r2r), ids1 ++ ids2)
    case StarExp(r, _) =>
      val (r1r, ids1) = fromRegExp(r)
      (Star(r1r), ids1)
    case GroupExp(r, id, _, _, _) =>
      val (r1r, ids1) = fromRegExp(r)
      (Concat(Atom(OpenParen(id)), Concat(r1r, Atom(CloseParen(id)))), Set(id) ++ ids1)
    case BackReferenceExp(id, _) => (Atom(Reference(id)), Set(id))
    case e => throw new Exception(s"FIXME: Conversion to Kleene regex is undefined: $e")
  }
}
