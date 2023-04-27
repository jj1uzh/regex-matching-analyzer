package matching.regexp

import scala.collection.mutable.ArrayBuffer
import scala.io.AnsiColor._

object RegExpStructureAnalysis {

  /** Analyze Structure of Regular Expressions. */
  sealed trait StructureAnalyzer[A, R <: RegExp[A]] {

    /** Name of this analyzer. */
    def name: String

    /** Analyze Regex `r`.
      * @return
      *   If `r` satisfies the condition then corresponding sub-regex. Otherwise None.
      */
    def analyze(r: R): Option[RegExp[A]]
  }

  final object HasLookbehind extends StructureAnalyzer[Char, RegExp[Char]] {
    def name = "Has lookbehinds?"
    def analyze(r: RegExp[Char]): Option[RegExp[Char]] =
      r match {
        case exp @ LookbehindExp(_, _) => Some(exp)
        case RegExpOp1(r1)             => analyze(r1)
        case RegExpOp2(r1, r2)         => analyze(r1) orElse analyze(r2)
        case RegExpOp3(r1, r2, r3)     => analyze(r1) orElse analyze(r2) orElse analyze(r3)
        case _                         => None
      }
  }

  final object HasLookbehindWithCapture extends StructureAnalyzer[Char, RegExp[Char]] {
    def name = "Has backreferenced captures inside lookbehinds?"
    def analyze(r: RegExp[Char]): Option[RegExp[Char]] = {
      (for (
        lb <- listLB(r);
        capt <- listCapt(lb);
        if captIsBackrefed(r, capt)
      ) yield capt).headOption
    }
    private def listLB(r: RegExp[Char]): LazyList[LookbehindExp[Char]] = {
      r match {
        case exp @ LookbehindExp(r, _) => exp #:: listLB(r)
        case RegExpOp1(r1)             => listLB(r1)
        case RegExpOp2(r1, r2)         => listLB(r1) ++ listLB(r2)
        case RegExpOp3(r1, r2, r3)     => listLB(r1) ++ listLB(r2) ++ listLB(r3)
        case _                         => LazyList.empty
      }
    }
    private def listCapt(r: RegExp[Char]): LazyList[GroupExp[Char]] = {
      r match {
        case exp @ GroupExp(r, _, _) => exp #:: listCapt(r)
        case RegExpOp1(r1)           => listCapt(r1)
        case RegExpOp2(r1, r2)       => listCapt(r1) ++ listCapt(r2)
        case RegExpOp3(r1, r2, r3)   => listCapt(r1) ++ listCapt(r2) ++ listCapt(r3)
        case _                       => LazyList.empty
      }
    }
    private def captIsBackrefed(r: RegExp[Char], exp: GroupExp[Char]): Boolean = {
      r match {
        case BackReferenceExp(id, _) => id == exp.id
        case RegExpOp1(r1)           => captIsBackrefed(r1, exp)
        case RegExpOp2(r1, r2)       => captIsBackrefed(r1, exp) || captIsBackrefed(r2, exp)
        case RegExpOp3(r1, r2, r3) =>
          captIsBackrefed(r1, exp) || captIsBackrefed(r2, exp) || captIsBackrefed(r3, exp)
        case _ => false
      }
    }
  }

  final object HasLookaheadWithCapture extends StructureAnalyzer[Char, RegExp[Char]] {
    def name = "Has backreferenced captures inside lookahead?"
    def analyze(r: RegExp[Char]): Option[RegExp[Char]] = {
      (for (
        lb <- listLB(r);
        capt <- listCapt(lb);
        if captIsBackrefed(r, capt)
      ) yield capt).headOption
    }
    private def listLB(r: RegExp[Char]): LazyList[LookaheadExp[Char]] = {
      r match {
        case exp @ LookaheadExp(r, _) => exp #:: listLB(r)
        case RegExpOp1(r1)            => listLB(r1)
        case RegExpOp2(r1, r2)        => listLB(r1) ++ listLB(r2)
        case RegExpOp3(r1, r2, r3)    => listLB(r1) ++ listLB(r2) ++ listLB(r3)
        case _                        => LazyList.empty
      }
    }
    private def listCapt(r: RegExp[Char]): LazyList[GroupExp[Char]] = {
      r match {
        case exp @ GroupExp(r, _, _) => exp #:: listCapt(r)
        case RegExpOp1(r1)           => listCapt(r1)
        case RegExpOp2(r1, r2)       => listCapt(r1) ++ listCapt(r2)
        case RegExpOp3(r1, r2, r3)   => listCapt(r1) ++ listCapt(r2) ++ listCapt(r3)
        case _                       => LazyList.empty
      }
    }
    private def captIsBackrefed(r: RegExp[Char], exp: GroupExp[Char]): Boolean = {
      r match {
        case BackReferenceExp(id, _) => id == exp.id
        case RegExpOp1(r1)           => captIsBackrefed(r1, exp)
        case RegExpOp2(r1, r2)       => captIsBackrefed(r1, exp) || captIsBackrefed(r2, exp)
        case RegExpOp3(r1, r2, r3) =>
          captIsBackrefed(r1, exp) || captIsBackrefed(r2, exp) || captIsBackrefed(r3, exp)
        case _ => false
      }
    }
  }

  final case class MarkedRegExp[A](child: RegExp[A]) extends RegExp[A] {
    override def toString(): String = s"${UNDERLINED}${child}${RESET}"
  }
  private def marked[A](r: RegExp[A], sub: RegExp[A]): RegExp[A] = {
    if (r == sub) MarkedRegExp(r) else recursiveApply(r, marked(_, sub))
  }

  final class StructureAnalyzersManager(
      analyzers: Seq[StructureAnalyzer[Char, RegExp[Char]]]
  ) {
    private val agg: Seq[ArrayBuffer[RegExp[Char]]] =
      Vector.fill(analyzers.size)(ArrayBuffer.empty)
    private val zipped = analyzers zip agg
    private var total = 0
    def analyze(r: RegExp[Char]): Unit = {
      total += 1
      zipped.foreach { case (analyzer, set) =>
        analyzer.analyze(r).foreach { subexp =>
          set += marked(r, subexp)
        }
      }
    }
    def printResult(): Unit = {
      println(s"Total: ${total}")
      zipped.foreach { case (analyzer, arr) =>
        println(s"* ${analyzer.name} (Total: ${arr.size})")
        arr.foreach { r => println(s"  ${r.toString}") }
      }
    }
  }
  object StructureAnalyzersManager {
    def apply(analyzers: StructureAnalyzer[Char, RegExp[Char]]*): StructureAnalyzersManager =
      new StructureAnalyzersManager(analyzers)
  }

  // TODO:tmp
  private def recursiveApply[A](r: RegExp[A], f: RegExp[A] => RegExp[A]): RegExp[A] = {
    def optConcatExp[A](r1: RegExp[A], r2: RegExp[A]): RegExp[A] = {
      (r1, r2) match {
        case (EpsExp(), _) => r2
        case (_, EpsExp()) => r1
        case (_, RepeatExp(`r1`, min, max, greedy)) =>
          RepeatExp(r1, min.orElse(Some(0)).map(_ + 1), max.map(_ + 1), greedy)
        case _ => ConcatExp(r1, r2)
      }
    }
    r match {
      case ConcatExp(r1, r2)              => optConcatExp(f(r1), f(r2))
      case AltExp(r1, r2)                 => AltExp(f(r1), f(r2))
      case StarExp(r, greedy)             => StarExp(f(r), greedy)
      case PlusExp(r, greedy)             => PlusExp(f(r), greedy)
      case OptionExp(r, greedy)           => OptionExp(f(r), greedy)
      case RepeatExp(r, min, max, greedy) => RepeatExp(f(r), min, max, greedy)
      case GroupExp(r, id, name)          => GroupExp(f(r), id, name)
      case LookaheadExp(r, positive)      => LookaheadExp(f(r), positive)
      case LookbehindExp(r, positive)     => LookbehindExp(f(r), positive)
      case _                              => r
    }
  }
}
