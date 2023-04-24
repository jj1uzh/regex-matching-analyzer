package matching.regexp

import scala.collection.mutable.ArrayBuffer

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
      captInLB(r).find(captIsBackrefed(r, _))
    }
    private def captInLB(r: RegExp[Char]): LazyList[GroupExp[Char]] = {
      r match {
        case exp @ GroupExp(r, _, _) => exp #:: captInLB(r)
        case RegExpOp1(r1)           => captInLB(r1)
        case RegExpOp2(r1, r2)       => captInLB(r1) ++ captInLB(r2)
        case RegExpOp3(r1, r2, r3)   => captInLB(r1) ++ captInLB(r2) ++ captInLB(r3)
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
        analyzer.analyze(r).foreach { subexp => set += r }
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
}
