package matching.regexp

object RegExpStructureAnalysis {

  sealed trait StructureAnalyzer[A, R <: RegExp[A]] {
    def name: String
    def analyze(r: R): Boolean
  }

  final object HasLookbehind extends StructureAnalyzer[Char, RegExp[Char]] {
    def name = "Has Lookbehind?"
    def analyze(r: RegExp[Char]): Boolean =
      r match {
        case LookbehindExp(_, _)   => true
        case RegExpOp1(r1)         => analyze(r1)
        case RegExpOp2(r1, r2)     => analyze(r1) || analyze(r2)
        case RegExpOp3(r1, r2, r3) => analyze(r1) || analyze(r2) || analyze(r3)
        case _                     => false
      }
  }

  final object HasLookbehindWithCapture extends StructureAnalyzer[Char, RegExp[Char]] {
    def name = "Has Lookbehind?"
    def analyze(r: RegExp[Char]): Boolean = {
      val backrefedCaptureIDs = capturesInsideLookbehind(r)
      backrefedCaptureIDs.iterator.filter(captureIsBackreferenced(r, _)).nonEmpty
    }
    private def capturesInsideLookbehind(r: RegExp[Char]): Set[Int] = {
      r match {
        case GroupExp(r, id, _) => Set(id) ++ capturesInsideLookbehind(r)
        case RegExpOp1(r1)      => capturesInsideLookbehind(r1)
        case RegExpOp2(r1, r2)  => capturesInsideLookbehind(r1) ++ capturesInsideLookbehind(r2)
        case RegExpOp3(r1, r2, r3) =>
          capturesInsideLookbehind(r1) ++
            capturesInsideLookbehind(r2) ++
            capturesInsideLookbehind(r3)
        case _ => Set()
      }
    }
    private def captureIsBackreferenced(r: RegExp[Char], id: Int): Boolean = {
      r match {
        case BackReferenceExp(n, _) => id == n
        case RegExpOp1(r1)          => captureIsBackreferenced(r1, id)
        case RegExpOp2(r1, r2) => captureIsBackreferenced(r1, id) || captureIsBackreferenced(r2, id)
        case RegExpOp3(r1, r2, r3) =>
          captureIsBackreferenced(r1, id) ||
          captureIsBackreferenced(r2, id) ||
          captureIsBackreferenced(r3, id)
        case _ => false
      }
    }
  }
}
