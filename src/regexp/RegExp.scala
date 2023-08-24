package matching.regexp

import matching.Witness
import matching.monad.AMonad._
import matching.monad.Monad._
import matching.monad.StateT._
import matching.monad.Tree._
import matching.tool.Analysis
import matching.tool.Debug
import matching.tool.IO
import matching.transition.BacktrackMethod.BDM
import matching.transition.BacktrackMethod.KM
import matching.transition._

import scala.collection.mutable.Stack

trait RegExp[A] {
  override def toString(): String = RegExp.toString(this)
  /*
  def derive[M[_,_]](a: A)(implicit deriver: RegExpDeriver[M])
    : M[Option[RegExp[A]], Option[RegExp[A]]] = deriver.derive(this,Some(a))
  def derive[M[_,_]](a: Option[A])(implicit deriver: RegExpDeriver[M])
    : M[Option[RegExp[A]], Option[RegExp[A]]] = deriver.derive(this,a)
  def deriveEOL[M[_,_]]()(implicit deriver: RegExpDeriver[M])
    : M[Unit, Unit] = deriver.deriveEOL(this)
   */
  def derive[M[_]](a: A)(implicit deriver: RegExpSetTreeDeriver) = deriver.derive(this, Some(a))
  def derive[M[_]](a: Option[A])(implicit deriver: RegExpSetTreeDeriver) = deriver.derive(this, a)
  def deriveEOL[M[_]]()(implicit deriver: RegExpSetTreeDeriver) = deriver.deriveEOL(this)
}

sealed trait RegExpLeaf[A] extends RegExp[A]

sealed class RegExpOp1[A](val child: RegExp[A]) extends RegExp[A]
object RegExpOp1 {
  def unapply[A](rr: RegExpOp1[A]): Option[RegExp[A]] =
    Some(rr.child)
}

sealed class RegExpOp2[A](val child1: RegExp[A], val child2: RegExp[A]) extends RegExp[A]
object RegExpOp2 {
  def unapply[A](rr: RegExpOp2[A]): Option[(RegExp[A], RegExp[A])] =
    Some(rr.child1, rr.child2)
}

sealed class RegExpOp3[A](val child1: RegExp[A], val child2: RegExp[A], val child3: RegExp[A])
    extends RegExp[A]
object RegExpOp3 {
  def unapply[A](rr: RegExpOp3[A]): Option[(RegExp[A], RegExp[A], RegExp[A])] =
    Some(rr.child1, rr.child2, rr.child3)
}

case class ElemExp[A](a: A) extends RegExpLeaf[A]
case class EmptyExp[A]() extends RegExpLeaf[A]
case class EpsExp[A]() extends RegExpLeaf[A]
case class ConcatExp[A](r1: RegExp[A], r2: RegExp[A]) extends RegExpOp2[A](r1, r2)
case class AltExp[A](r1: RegExp[A], r2: RegExp[A]) extends RegExpOp2[A](r1, r2)
case class StarExp[A](r: RegExp[A], greedy: Boolean) extends RegExpOp1[A](r)
case class PlusExp[A](r: RegExp[A], greedy: Boolean) extends RegExpOp1[A](r)
case class OptionExp[A](r: RegExp[A], greedy: Boolean) extends RegExpOp1[A](r)
case class RepeatExp[A](r: RegExp[A], var min: Option[Int], var max: Option[Int], greedy: Boolean)
    extends RegExpOp1[A](r)
object RepeatExp {
  def apply[A](r: RegExp[A], min: Option[Int], max: Option[Int], greedy: Boolean): RegExp[A] = {
    def validate(): Unit = {
      (min, max) match {
        case (Some(min), Some(max)) =>
          if (min < 0 || max < 0) {
            throw RegExp.InvalidRegExpException(
              s"invalid repeat expression: min and max must be positive"
            )
          } else if (min > max) {
            throw RegExp.InvalidRegExpException(
              s"invalid repeat expression: ${min} is larger than ${max}"
            )
          }
        case (Some(min), None) =>
          if (min < 0) {
            throw RegExp.InvalidRegExpException(
              s"invalid repeat expression: min and max must be positive"
            )
          }
        case (None, Some(max)) =>
          if (max < 0) {
            throw RegExp.InvalidRegExpException(
              s"invalid repeat expression: min and max must be positive"
            )
          }
        case (None, None) =>
          throw RegExp.InvalidRegExpException(
            "invalid repeat expression: either min or max must be specified"
          )
      }
    }

    validate()

    (min, max) match {
      case (_, Some(0))    => EpsExp()
      case (Some(0), None) => StarExp(r, greedy)
      case (Some(0), max)  => new RepeatExp(r, None, max, greedy)
      case _               => new RepeatExp(r, min, max, greedy)
    }
  }
}

/** Capture Group
  * @param r
  *   Sub-expression.
  * @param id
  *   Capture ID.
  * @param name
  *   Capture name.
  * @param referenced
  *   Whether this capture is referenced by some BackReferencedExps. None if unkonwn.
  * @param unique
  *   Whether the ID is unique. None if unknown.
  */
case class GroupExp[A](
    r: RegExp[A],
    id: Int,
    name: Option[String],
    referenced: Option[Boolean] = None,
    unique: Option[Boolean] = None
) extends RegExpOp1[A](r)

case class BackReferenceExp[A](n: Int, name: Option[String]) extends RegExpLeaf[A]
case class StartAnchorExp[A]() extends RegExpLeaf[A]
case class EndAnchorExp[A]() extends RegExpLeaf[A]
case class LookaheadExp[A](r: RegExp[A], positive: Boolean) extends RegExpOp1[A](r)
case class LookbehindExp[A](r: RegExp[A], positive: Boolean) extends RegExpOp1[A](r)
case class IfExp[A](cond: RegExp[A], rt: RegExp[A], rf: RegExp[A])
    extends RegExpOp3[A](cond, rt, rf)
case class DotExp[A]() extends RegExpLeaf[A]
case class CharClassExp(es: Seq[CharClassElem], positive: Boolean) extends RegExpLeaf[Char]
case class MetaCharExp(c: Char) extends RegExpLeaf[Char] with CharClassElem {
  val negetiveChar = Set('D', 'H', 'S', 'V', 'W')

  val charSet = c match {
    case 'd' | 'D' => ('0' to '9').toSet
    case 'h' | 'H' => Set('\u0009')
    case 'R'       => Set('\r', '\n')
    case 's' | 'S' => Set(' ', '\t', '\n', '\r', '\f')
    case 'v' | 'V' => Set('\u000B')
    case 'w' | 'W' => ('a' to 'z').toSet | ('A' to 'Z').toSet | ('0' to '9').toSet + '_'

    case _ => throw RegExp.InvalidRegExpException(s"invalid meta character: \\${c}")
  }

  val negative = negetiveChar(c)
}
case class BoundaryExp() extends RegExpLeaf[Char]

case class FailEpsExp[A]() extends RegExpLeaf[A]

//for modify in backreference
case class MTreeExp[A](r: RegExp[A]) extends RegExpOp1[A](r)

case class MTreePrimeExp[A](r: RegExp[A]) extends RegExpOp1[A](r)

object RegExp {
  case class InvalidRegExpException(message: String) extends Exception(message: String)

  class PCREOptions(s: String = "") {
    var ignoreCase = false
    var dotAll = false

    var ungreedy = false

    s.foreach {
      case 'i' => ignoreCase = true
      case 's' => dotAll = true
      case 'U' => ungreedy = true
      case c   => throw RegExp.InvalidRegExpException(s"unsupported PCRE option: ${c}")
    }
  }

  def toString[A](r: RegExp[A]): String = {
    r match {
      case ElemExp(a) => IO.escape(a)
      case EmptyExp() => "∅"
      case EpsExp()   => "ε"
      case ConcatExp(r1, r2) =>
        val reg1 = r1 match {
          case AltExp(_, _) => s"(?:${r1})"
          case _            => r1.toString
        }
        val reg2 = r2 match {
          case ConcatExp(_, _) | AltExp(_, _) => s"(?:${r2})"
          case _                              => r2.toString
        }
        s"${reg1}${reg2}"
      case AltExp(r1, r2) =>
        r2 match {
          case AltExp(_, _) => s"${r1}|(?:${r2})"
          case _            => s"${r1}|${r2}"
        }
      case StarExp(r, greedy) =>
        r match {
          case ConcatExp(_, _) | AltExp(_, _) | StarExp(_, _) | PlusExp(_, _) | OptionExp(_, _) |
              RepeatExp(_, _, _, _) =>
            s"(?:${r})*${if (greedy) "" else "?"}"
          case _ => s"${r}*${if (greedy) "" else "?"}"
        }
      case PlusExp(r, greedy) =>
        r match {
          case ConcatExp(_, _) | AltExp(_, _) | StarExp(_, _) | PlusExp(_, _) | OptionExp(_, _) |
              RepeatExp(_, _, _, _) =>
            s"(?:${r})+${if (greedy) "" else "?"}"
          case _ => s"${r}+${if (greedy) "" else "?"}"
        }
      case OptionExp(r, greedy) =>
        r match {
          case ConcatExp(_, _) | AltExp(_, _) | StarExp(_, _) | PlusExp(_, _) | OptionExp(_, _) |
              RepeatExp(_, _, _, _) =>
            s"(?:${r})?${if (greedy) "" else "?"}"
          case _ => s"${r}?${if (greedy) "" else "?"}"
        }
      case DotExp() => "."
      case RepeatExp(r, min, max, greedy) =>
        val reg = r match {
          case ConcatExp(_, _) | AltExp(_, _) | StarExp(_, _) | PlusExp(_, _) | OptionExp(_, _) |
              RepeatExp(_, _, _, _) =>
            s"(?:${r})"
          case _ => r.toString
        }
        val rep = if (min == max) {
          s"{${min.get}}${if (greedy) "" else "?"}"
        } else {
          s"{${min.getOrElse("")},${max.getOrElse("")}}${if (greedy) "" else "?"}"
        }
        s"${reg}${rep}"
      case CharClassExp(es, positive) => s"[${if (positive) "" else "^"}${es.mkString}]"
      case MetaCharExp(c)             => s"\\${c}"
      case BoundaryExp()              => "\\b"
      case GroupExp(r, _, name, _, _) =>
        name match {
          case Some(name) => s"(?<${name}>${r})"
          case None       => s"(${r})"
        }
      case StartAnchorExp() => "^"
      case EndAnchorExp()   => "$"
      case BackReferenceExp(n, name) =>
        name match {
          case Some(name) => s"(?P=${name})"
          case None       => s"\\${n}"
        }
      case LookaheadExp(r, positive)  => s"(?${if (positive) "=" else "!"}${r})"
      case LookbehindExp(r, positive) => s"(?<${if (positive) "=" else "!"}${r})"
      case IfExp(cond, rt, rf)        => s"(?(${cond})${rt}|${rf})"
      case FailEpsExp()               => "\u25C7"

      case MTreeExp(r) => s"(○${r})"
    }
  }

  def optConcatExp[A](r1: RegExp[A], r2: RegExp[A]): RegExp[A] = {
    (r1, r2) match {
      case (EpsExp(), _) => r2
      case (_, EpsExp()) => r1
      case (_, RepeatExp(`r1`, min, max, greedy)) =>
        RepeatExp(r1, min.orElse(Some(0)).map(_ + 1), max.map(_ + 1), greedy)
      case _ => ConcatExp(r1, r2)
    }
  }

  def modifyRegExp[A](r: RegExp[A]): (RegExp[A], Boolean) = {
    var approximated = false

    def recursiveApply(r: RegExp[A], f: RegExp[A] => RegExp[A]): RegExp[A] = {
      r match {
        case ConcatExp(r1, r2)              => optConcatExp(f(r1), f(r2))
        case AltExp(r1, r2)                 => AltExp(f(r1), f(r2))
        case StarExp(r, greedy)             => StarExp(f(r), greedy)
        case PlusExp(r, greedy)             => PlusExp(f(r), greedy)
        case OptionExp(r, greedy)           => OptionExp(f(r), greedy)
        case RepeatExp(r, min, max, greedy) => RepeatExp(f(r), min, max, greedy)
        case GroupExp(r, id, name, _, _)    => GroupExp(f(r), id, name)
        case LookaheadExp(r, positive)      => LookaheadExp(f(r), positive)
        case LookbehindExp(r, positive)     => LookbehindExp(f(r), positive)
        case _                              => r
      }
    }

    def checkSupported(r: RegExp[A], inLookAhead: Boolean = false): Unit = {
      def isBounded(r: RegExp[A]): Unit = {
        r match {
          case StarExp(_, _) | PlusExp(_, _) | BackReferenceExp(_, _) =>
            throw RegExp.InvalidRegExpException(
              s"lookbehind with unbounded matching length is unsupported."
            )
          case ConcatExp(r1, r2) => isBounded(r1); isBounded(r2)
          case AltExp(r1, r2)    => isBounded(r1); isBounded(r2)
          case OptionExp(r, _)   => isBounded(r)
          case RepeatExp(r, _, max, _) =>
            if (max.isDefined) {
              isBounded(r)
            } else {
              throw RegExp.InvalidRegExpException(
                s"lookbehind with unbounded matching length is unsupported."
              )
            }
          case GroupExp(r, _, _, _, _) => isBounded(r)
          case LookaheadExp(r, _)      => isBounded(r)
          case LookbehindExp(r, _)     => isBounded(r)
          case _                       => // NOP
        }
      }

      r match {
        case ConcatExp(r1, r2) => checkSupported(r1, inLookAhead); checkSupported(r2, inLookAhead)
        case AltExp(r1, r2)    => checkSupported(r1, inLookAhead); checkSupported(r2, inLookAhead)
        case StarExp(r, _)     => checkSupported(r, inLookAhead)
        case PlusExp(r, _)     => checkSupported(r, inLookAhead)
        case OptionExp(r, _)   => checkSupported(r, inLookAhead)
        case RepeatExp(r, _, _, _)   => checkSupported(r, inLookAhead)
        case GroupExp(r, _, _, _, _) => checkSupported(r, inLookAhead)
        case BackReferenceExp(_, _) if inLookAhead =>
          throw RegExp.InvalidRegExpException(s"back reference in lookahead is unsupported.")
        case LookaheadExp(r, _) => checkSupported(r, true)
        case LookbehindExp(r, _) =>
          if (inLookAhead) {
            throw RegExp.InvalidRegExpException(s"lookbehind in lookahead is unsupported.")
          } else {
            checkSupported(r, inLookAhead); isBounded(r)
          }
        case IfExp(_, _, _) =>
          throw RegExp.InvalidRegExpException(s"conditional expression is unsupported.")
        case BoundaryExp() if inLookAhead =>
          throw RegExp.InvalidRegExpException(s"word boundary in lookahead is unsupported.")
        case _ => // NOP
      }
    }

    def isStartAnchorHead(r: RegExp[A]): Boolean = {
      r match {
        case StartAnchorExp() => true
        case ConcatExp(r1, _) => isStartAnchorHead(r1)
        case AltExp(r1, r2)   => isStartAnchorHead(r1) && isStartAnchorHead(r2)
        case PlusExp(r, _)    => isStartAnchorHead(r)
        case RepeatExp(r, min, _, _) if min.isDefined => isStartAnchorHead(r)
        case GroupExp(r, _, _, _, _)                  => isStartAnchorHead(r)
        case _                                        => false
      }
    }

    def getGroupMap(r: RegExp[A]): Map[Int, RegExp[A]] = {
      r match {
        case GroupExp(r, n, _, _, _)                => Map(n -> r) ++ getGroupMap(r)
        case ConcatExp(r1, r2)                      => getGroupMap(r1) ++ getGroupMap(r2)
        case AltExp(r1, r2)                         => getGroupMap(r1) ++ getGroupMap(r2)
        case StarExp(r, _)                          => getGroupMap(r)
        case PlusExp(r, _)                          => getGroupMap(r)
        case OptionExp(r, _)                        => getGroupMap(r)
        case RepeatExp(r, _, _, _)                  => getGroupMap(r)
        case LookaheadExp(r, positive) if positive  => getGroupMap(r)
        case LookbehindExp(r, positive) if positive => getGroupMap(r)
        case _                                      => Map()
      }
    }

    def collectGroupsInPositiveLookAround(r: RegExp[A]): Set[Int] = {
      r match {
        case GroupExp(r, _, _, _, _) => collectGroupsInPositiveLookAround(r)
        case ConcatExp(r1, r2) =>
          collectGroupsInPositiveLookAround(r1) ++ collectGroupsInPositiveLookAround(r2)
        case AltExp(r1, r2) =>
          collectGroupsInPositiveLookAround(r1) ++ collectGroupsInPositiveLookAround(r2)
        case StarExp(r, _)                          => collectGroupsInPositiveLookAround(r)
        case PlusExp(r, _)                          => collectGroupsInPositiveLookAround(r)
        case OptionExp(r, _)                        => collectGroupsInPositiveLookAround(r)
        case RepeatExp(r, _, _, _)                  => collectGroupsInPositiveLookAround(r)
        case LookaheadExp(r, positive) if positive  => getGroupMap(r).keySet
        case LookbehindExp(r, positive) if positive => getGroupMap(r).keySet
        case _                                      => Set()
      }
    }

    def collectBackReferences(r: RegExp[A]): Set[Int] = {
      r match {
        case ConcatExp(r1, r2)       => collectBackReferences(r1) | collectBackReferences(r2)
        case AltExp(r1, r2)          => collectBackReferences(r1) | collectBackReferences(r2)
        case StarExp(r, _)           => collectBackReferences(r)
        case PlusExp(r, _)           => collectBackReferences(r)
        case OptionExp(r, _)         => collectBackReferences(r)
        case RepeatExp(r, _, _, _)   => collectBackReferences(r)
        case GroupExp(r, _, _, _, _) => collectBackReferences(r)
        case BackReferenceExp(n, _)  => Set(n)
        case _                       => Set()
      }
    }

    def replace(r: RegExp[A], groupMap: Map[Int, RegExp[A]]): RegExp[A] = {
      def replace(r: RegExp[A]): RegExp[A] = {
        def removeAssert(r: RegExp[A]): RegExp[A] = {
          r match {
            case LookaheadExp(_, _) | LookbehindExp(_, _) | StartAnchorExp() | EndAnchorExp() |
                BoundaryExp() =>
              EpsExp()
            case _ => recursiveApply(r, removeAssert)
          }
        }

        r match {
          case GroupExp(r, _, _, _, _) => replace(r)
          case LookbehindExp(_, _) | BoundaryExp() =>
            approximated = true
            FailEpsExp()
          case BackReferenceExp(n, _) =>
            approximated = true
            // ConcatExp(replace(removeAssert(groupMap(n))), FailEpsExp())
            MTreeExp(replace(groupMap(n)))
          case _ => recursiveApply(r, replace)
        }
      }

      replace(r)
    }

    checkSupported(r)

    // simulates suffix match
    val r1 = if (isStartAnchorHead(r)) r else ConcatExp(StarExp(DotExp(), false), r)

    // check back reference to group in positive lookaround
    val groupsInPositiveLookAround = collectGroupsInPositiveLookAround(r1)
    val backReferences = collectBackReferences(r1)

    if ((groupsInPositiveLookAround & backReferences).nonEmpty) {
      throw RegExp.InvalidRegExpException(
        s"back reference to group in positive lookaround is unsupported."
      )
    }

    // check dependencies of back references
    val groupMap = getGroupMap(r1).withDefaultValue(EmptyExp())
    var dependencyMap = groupMap.view.mapValues(collectBackReferences)

    while (dependencyMap.nonEmpty) {
      val newDependencyMap = dependencyMap.filter(_._2.nonEmpty)
      if (dependencyMap.keySet == newDependencyMap.keySet) {
        throw RegExp.InvalidRegExpException(s"back reference with cycle is unsupported.")
      } else {
        val newKeys = newDependencyMap.keySet
        dependencyMap = newDependencyMap.mapValues(_.filter(newKeys))
      }
    }

    // approximate lookbehind/back reference
    val r2 = replace(r1, groupMap)

    (r2, approximated)
  }
  def constructTransducer(
      r: RegExp[Char],
      options: PCREOptions = new PCREOptions()
  ): NonDetNoAssertTreeTransducer[(Boolean, Option[(RegExp[Char], Boolean)]), Option[Char]] = {
    def getElems(r: RegExp[Char]): Set[Char] = {
      r match {
        case ElemExp(a)        => if (options.ignoreCase && a.isLetter) Set(a.toLower) else Set(a)
        case ConcatExp(r1, r2) => getElems(r1) | getElems(r2)
        case AltExp(r1, r2)    => getElems(r1) | getElems(r2)
        case StarExp(r, _)     => getElems(r)
        case PlusExp(r, _)     => getElems(r)
        case OptionExp(r, _)   => getElems(r)
        case RepeatExp(r, _, _, _) => getElems(r)
        case LookaheadExp(r, _)    => getElems(r)
        case CharClassExp(es, _) =>
          val s = es.flatMap(_.charSet).toSet
          if (options.ignoreCase) {
            s.map {
              case a if a.isLetter => a.toLower
              case a               => a
            }
          } else s
        case DotExp() => if (options.dotAll) Set() else Set('\n')
        case r @ MetaCharExp(_) =>
          val s = r.charSet
          if (options.ignoreCase) {
            s.map {
              case a if a.isLetter => a.toLower
              case a               => a
            }
          } else s
        case _ => Set()
      }
    }

    val sigma =
      getElems(r).map(
        Option(_)
      ) + None // None: character which does not appear in the given expression
    val initialState: (RegExp[Char], Boolean) = (r, true)
    var states = Set(initialState)
    val stack = Stack(initialState)
    var delta = Map[
      ((RegExp[Char], Boolean), Option[Option[Char]]),
      SetTree[(RegExp[Char], Boolean)]
    ]()
    implicit val deriver = new RegExpSetTreeDeriver(options)

    while (stack.nonEmpty) {
      val s @ (r, u) = stack.pop()
      sigma.foreach { a =>
        Analysis.checkInterrupted("regular expression -> transducer")
        val t = (r.derive(a) >>= {
          case Some(r) => StateTSetTreeMonad[RegExp[Char]](r)
          case None    => StateTSetTreeMonad.success[RegExp[Char]] // simulates prefix match
        })(u)
        delta += (s, Some(a)) -> t
        for (tree <- t) {
          Analysis.checkInterrupted("regular expression -> transducer")
          val newExps = flat(tree).filterNot(states.contains)
          states |= newExps.toSet
          stack.pushAll(newExps)
        }
      }
      val t = (r.deriveEOL() >>= (_ => StateTSetTreeMonad.success[RegExp[Char]]))(u)
      delta += (s, None) -> t
    }
    new NonDetNoAssertTreeTransducer(states, sigma, initialState, delta).Bprune()
  }

  type typeOfTD = Option[Char]
  type typeOfTD2 = Either[Option[Char], Int]

  def calcTimeComplexity(
      r: RegExp[Char],
      options: PCREOptions,
      method: Option[BacktrackMethod]
  ): (
      Option[Int],
      Witness[Char],
      Boolean,
      Int
  ) // (degree, witness, approximated?, size of transducer)
  = {
    def convertWitness_KM(w: Witness[typeOfTD]): Witness[Char] = {
      val charForNone = '\u25AE'
      def convertNone(e: typeOfTD): Seq[Char] = {
        e match {
          case None    => Seq(charForNone)
          case Some(c) => Seq(c)
          case _       => Seq.empty
        }

      }
      Witness(w.separators.map(_.flatMap(convertNone)), w.pumps.map(_.flatMap(convertNone)))
    }
    def convertWitness_BDM(w: Witness[typeOfTD2]): Witness[Char] = {
      val charForNone = '\u25AE'
      def convertNone(e: typeOfTD2): Seq[Char] = {
        e match {
          case (Left(None))    => Seq(charForNone)
          case (Left(Some(c))) => Seq(c)
          case _               => Seq.empty
        }
      }
      Witness(w.separators.map(_.flatMap(convertNone)), w.pumps.map(_.flatMap(convertNone)))
    }

    val (rm, approximated) = modifyRegExp(r)

    val transducer_KM = Debug.time("regular expression -> transducer") {
      constructTransducer(rm, options).rename()
    }

    method match {
      // KM:Kawamura,Minamide
      case Some(KM) =>
        val (growthRate, witness) = transducer_KM.calcGrowthRate()
        (growthRate, convertWitness_KM(witness), approximated, transducer_KM.delta.size)
      // BDM:Berglund,Drewes,van der Merwe
      case Some(BDM) =>
        val transducer_BDM = Debug.time("regular expression -> transducer") {
          transducer_KM.toDeterministic().toTotal().rename()
        }

        val (growthRate, witness) = transducer_BDM.calcGrowthRate()

        (growthRate, convertWitness_BDM(witness), approximated, transducer_BDM.delta.size)
      case _ => throw new Exception(s"internal error.")
    }

  }
}

sealed trait CharClassElem {
  val charSet: Set[Char]
  override def toString(): String = CharClassElem.toString(this)
}

case class SingleCharExp(c: Char) extends CharClassElem {
  val charSet = Set(c)
}
case class RangeExp(start: Char, end: Char) extends CharClassElem {
  val charSet = (start to end).toSet
}

object CharClassElem {
  def toString(e: CharClassElem): String = {
    e match {
      case SingleCharExp(c)     => IO.escape(c)
      case RangeExp(start, end) => s"${IO.escape(start)}-${IO.escape(end)}"
      case MetaCharExp(c)       => s"\\${c}"
    }
  }
}
