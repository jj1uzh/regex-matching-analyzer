package matching.transition

import matching.regexp.KleeneRegex
import matching.tool.Analysis

import scala.collection.mutable

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
    val states = mutable.Set[Q]()
    val delta = Set.newBuilder[(Q, A, Q)]
    val unchecked = mutable.Set[Q](r)
    while (unchecked.nonEmpty) {
      val s = unchecked.head
      states.addOne(s)
      unchecked -= s
      sigma.foreach { a =>
        val nexts = s.deriv(a)
        delta.addAll(nexts.map((s, a, _)))
        unchecked ++= (nexts -- states)
      }
    }
    val deltaMap =
      delta
        .result()
        .groupMap { case (p, a, _) => (p, a) } { case (_, _, q) => q }
        .view
        .mapValues { case set if set.size == 1 => set.head }
        .toMap
    new DFA(
      states.to(Set),
      sigma,
      deltaMap,
      r,
      states.filter(_.nullable).to(Set)
    )
  }
}
