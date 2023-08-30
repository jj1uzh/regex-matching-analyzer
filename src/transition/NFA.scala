package matching.transition

import matching.regexp.KleeneRegex
import matching.tool.Analysis

import scala.collection.mutable.Stack

class NFA[Q, A](
    val states: Set[Q],
    val sigma: Set[A],
    val delta: Seq[(Q, A, Q)],
    val epsDelta: Seq[(Q, Q)],
    val initialStates: Set[Q],
    val finalStates: Set[Q],
) extends LabeledGraph[Q, A](states, delta) {
  override def reverse(): NFA[Q, A] = {
    new NFA(
      states,
      sigma,
      delta.map { case (q1, a, q2) => (q2, a, q1) },
      epsDelta.map(_.swap),
      finalStates,
      initialStates
    )
  }

  lazy val deltaSet =
    delta
      .groupMap { case (from, a, _) => (from, a) } { case (_, _, to) => to }
      .map { case (key, to) => key -> to.flatMap(epsClosure) }
      .withDefaultValue(Seq[Q]())

  lazy val epsClosure: Map[Q, Set[Q]] = {
    val map = scala.collection.mutable.Map[Q, Set[Q]]()
    val epsDeltaSet = epsDelta.toSet
    def impl(q: Q): Set[Q] = {
      map.get(q) match {
        case Some(s) => s
        case None =>
          val res = epsDeltaSet.filter(_._1 == q).flatMap { case (_, to) => impl(to) }
          map(q) = res
          res
      }
    }
    states.foreach(impl)
    map.toMap
  }

  def deltaHat(qs: Set[Q], a: A): Set[Q] = {
    qs.flatMap(q => deltaSet((q, a)))
  }

  def toDFA(): DFA[Set[Q], A] = {
    val newInitial = initialStates
    var newStates = Set(initialStates)
    val stack = Stack(newInitial)
    var newDelta = Map[(Set[Q], A), Set[Q]]()

    while (stack.nonEmpty) {
      Analysis.checkInterrupted("constructing DFA")
      val qs = stack.pop()
      sigma.foreach { a =>
        val next = deltaHat(qs, a)
        newDelta += (qs, a) -> next
        if (!newStates.contains(next)) {
          newStates += next
          stack.push(next)
        }
      }
    }

    val newFinal = newStates.filter(qs => (qs & finalStates).nonEmpty)

    new DFA(newStates, sigma, newDelta, newInitial, newFinal)
  }
}

object NFA {

  class NFABuilder[A] {
    private type NFA = matching.transition.NFA[Int, A]
    def renamed(n: NFA, fromNum: Int): (NFA, Int) = {
      val mapping = n.states.zip(Iterator from fromNum).toMap
      val nfa = new NFA(
        n.states.map(mapping),
        n.sigma,
        n.delta.map { case (s1, a, s2) => (mapping(s1), a, mapping(s2)) },
        n.epsDelta.map { case (s1, s2) => (mapping(s1), mapping(s2)) },
        n.initialStates.map(mapping),
        n.finalStates.map(mapping)
      )
      (nfa, mapping.values.max + 1)
    }
    def union(n1: NFA, n2: NFA): NFA = {
      val (n1r, k) = renamed(n1, fromNum = 1)
      val (n2r, _) = renamed(n2, fromNum = k)
      val init = 0
      new NFA(
        n1r.states ++ n2r.states + init,
        n1r.sigma ++ n2r.sigma,
        n1r.delta ++ n2r.delta,
        n1r.epsDelta ++ n2r.epsDelta ++
          n1r.initialStates.map(init -> _).toSeq ++
          n2r.initialStates.map(init -> _).toSeq,
        Set(init),
        n1r.finalStates ++ n2r.finalStates
      )
    }
    def concat(n1: NFA, n2: NFA): NFA = {
      val (n1r, k) = renamed(n1, fromNum = 0)
      val (n2r, _) = renamed(n2, fromNum = k)
      val concDelta = for (from <- n1r.finalStates; to <- n2r.initialStates) yield (from -> to)
      new NFA(
        n1r.states ++ n2r.states,
        n1r.sigma ++ n2r.sigma,
        n1r.delta ++ n2r.delta,
        n1r.epsDelta ++ n2r.epsDelta ++ concDelta.toSeq,
        n1r.initialStates ++ n2r.initialStates,
        n1r.finalStates ++ n2r.finalStates
      )
    }
    def star(n: NFA): NFA = {
      val (n1r, _) = renamed(n, fromNum = 1)
      val init = 0
      val initDelta = n1r.initialStates.map(init -> _).toSeq
      val loopDelta = n1r.finalStates.map(_ -> init).toSeq
      new NFA(
        n1r.states + init,
        n1r.sigma,
        n1r.delta,
        n1r.epsDelta ++ loopDelta ++ initDelta,
        Set(init),
        Set(init)
      )
    }
  }

  def fromKleeneRegex[A](r: KleeneRegex[A]): NFA[Int, A] = {
    val b = new NFABuilder[A]
    def impl(r: KleeneRegex[A]): NFA[Int, A] = r match {
      case KleeneRegex.Empty   => new NFA(Set(), Set(), Seq(), Seq(), Set(), Set())
      case KleeneRegex.Epsilon => new NFA(Set(0), Set(), Seq(), Seq(), Set(0), Set(0))
      case KleeneRegex.Atom(a) => new NFA(Set(0, 1), Set(a), Seq((0, a, 1)), Seq(), Set(0), Set(1))
      case KleeneRegex.Concat(r1, r2) => b.concat(impl(r1), impl(r2))
      case KleeneRegex.Union(r1, r2)  => b.union(impl(r1), impl(r2))
      case KleeneRegex.Star(r)        => b.star(impl(r))
    }
    impl(r)
  }
}
