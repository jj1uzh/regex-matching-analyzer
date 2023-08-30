package matching

import matching.regexp.KleeneRegex
import matching.regexp.KleeneRegex.ExtendedChar._
import matching.regexp.RegExp
import matching.tool.Debug
import matching.tool.Visualizer
import matching.transition.DFA
import matching.transition.SST

object CopyLevelAnalyzer {

  sealed trait Result
  case object Copyless extends Result
  case object BoundedCopy extends Result
  case class PolynomialCopy(growthRate: Int) extends Result
  case object Exponential extends Result

  def exec(r: RegExp[Char]): Unit = {
    val (kregex, idSet) = KleeneRegex.fromRegExp(r)
    Debug.debug(kregex)
    val dfa = DFA.fromKleeneRegexByDerivative(kregex)
    Debug.debug(dfa)
    new Visualizer.NFAVisualizer(dfa).visualize("tmp_dfa")
    val sst = SST.referenceDSST(idSet, dfa.sigma.collect { case WrappedChar(c) => c }).prod(dfa)
    Visualizer.SSTVisualizer(sst).visualize("tmp_sst")
    Debug.debug(sst)
    println(sst.growthRate)
  }
}
