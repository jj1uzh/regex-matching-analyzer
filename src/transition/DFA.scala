package matching.transition

import matching.regexp.KleeneRegex
import matching.regexp.KleeneRegex.Empty
import matching.regexp.KleeneRegex.Union
import matching.tool.Analysis

class DFA[Q, A](
    states: Set[Q],
    sigma: Set[A],
    val deltaDet: Map[(Q, A), Q],
    val initialState: Q,
    finalStates: Set[Q]
) extends NFA[Q, A](
      states,
      sigma,
      deltaDet.map { case ((v1, a), v2) =>
        Analysis.checkInterrupted("construct DFA")
        (v1, a, v2)
      }.toSeq,
      Seq(),
      Set(initialState),
      finalStates
    ) {

  override def toString(): String = {
    s"DFA(\n  states = ${states}\n  init = ${initialStates}\n  final = ${finalStates}\n  trans = ${deltaDet}\n)"
  }
}

object DFA {

  def fromKleeneRegexByDerivative[A](r: KleeneRegex[A]): DFA[KleeneRegex[A], A] = {
    type Q = KleeneRegex[A]
    val sigma = r.alphabets
    def goto(q: Q, a: A, visited: Set[Q], trans: Map[(Q, A), Q]): (Set[Q], Map[(Q, A), Q]) = {
      val d = q.deriv(a)
      val isVisited = d match {
        case u @ Union(r1, r2) => (visited contains u) || (visited contains Union(r2, r1))
        case other             => visited contains other
      }
      if (isVisited)
        (visited, trans + ((q, a) -> d))
      else
        explore(d, visited + d, trans + ((q, a) -> d))
    }
    def explore(q: Q, visited: Set[Q], trans: Map[(Q, A), Q]): (Set[Q], Map[(Q, A), Q]) = {
      sigma.foldLeft((visited, trans)) { case ((states, trans), a) =>
        goto(q, a, states, trans)
      }
    }
    val (states, trans) = explore(r, Set(r), Map())
    val trimedStates = states - Empty
    val trimedTrans = trans.filter { case ((q1, _), q2) => q1 != Empty && q2 != Empty }
    new DFA(trimedStates, sigma, trimedTrans, r, states.filter(_.nullable).to(Set))
  }
}
