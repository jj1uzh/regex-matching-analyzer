package matching.transition

import matching.regexp.KleeneRegex.ExtendedChar
import matching.regexp.KleeneRegex.ExtendedChar._
import matching.tool.PrettyPrint
import matching.tool.Visualizer
import matching.transition.SST._

/** Streaming String Transducer */
case class SST[State, InputAlph, OutputAlph](
    vars: Set[Var],
    varOut: Var,
    initState: State,
    accepts: Set[State],
    trans: Set[(State, InputAlph, Update[OutputAlph], State)],
) extends PrettyPrint {
  require(vars contains varOut)

  def states: Set[State] =
    trans.flatMap { case (s1, _, _, s2) => Set(s1, s2) }

  def prod[DFAState](
      dfa: DFA[DFAState, InputAlph]
  ): SST[(State, DFAState), InputAlph, OutputAlph] = {
    val zippedTrans =
      for (
        (fromState, in, update, toState) <- this.trans;
        ((fromDFAstate, dfaIn), toDFAstate) <- dfa.deltaDet;
        if in == dfaIn
      ) yield ((fromState, fromDFAstate), in, update, (toState, toDFAstate))
    val acceptingStates =
      for (q1 <- this.accepts; q2 <- dfa.finalStates) yield (q1, q2)
    SST(
      vars,
      varOut,
      initState = (this.initState, dfa.initialState),
      acceptingStates,
      zippedTrans
    ).trim
  }

  private def transMap_domain: Map[InputAlph, Set[(State, Update[OutputAlph], State)]] =
    trans
      .groupMap { case (_, in, _, _) => in } { case (s1, _, update, s2) => (s1, update, s2) }
      .withDefaultValue(Set())
  //  private def states: Set[State] = trans.flatMap { case (s1, _, _, s2) => Set(s1, s2) }
  private def transMap_ToState: Map[State, Set[(State, InputAlph, Update[OutputAlph])]] =
    trans
      .groupMap { case (_, _, _, to) => to } { case (from, in, update, _) => (from, in, update) }
      .withDefaultValue(Set())

  /** Calculate INF = {(p, x) in PxX; (p, x) is co-accessible and val(p, x) is infinite.} */
  private def infSet: Set[(State, Var)] = {
    val basecase = accepts.map(_ -> varOut)
    Iterator
      .iterate((basecase, basecase)) { case (all, prevAdded) =>
        val added = prevAdded
          .groupMap(_._1)(_._2)
          .flatMap { case (toState, assignees) =>
            // if (q', x') in INF, (q, x) s.t. q -> q' with [x' := ...x...] is in INF.
            transMap_ToState(toState).flatMap { case (fromState, _, update) =>
              update.varsDependsOn(assignees).map(fromState -> _)
            }
          }
          .toSet
        (added ++ all, added -- all)
      }
      .find { case (_, added) => added.isEmpty }
      .get
      ._1
  }

  def growthRate: Option[Int] = {
    val infSet = this.infSet
    if (infSet.isEmpty) return None
    val dt0l = {
      val morphs = transMap_domain.map { case (in, trans) =>
        in -> {
          trans
            .flatMap { case (fromState, update, toState) =>
              update.mapping.map { case (fromVar, form) => (fromState, fromVar) -> (form, toState) }
            }
            .groupMap(_._1)(_._2)
            .filter { case (pair, _) => infSet contains pair }
            .view
            .mapValues { to =>
              to.toSeq.flatMap { case (form, toState) =>
                form.collect { case v: Var if infSet contains (toState, v) => (toState, v) }
              }
            }
            .toMap
        }
      }
      new DT0L(infSet, morphs)
    }
    System.err.println(dt0l)
    dt0l.calcGrowthRate(infSet)._1
//    dt0l.calcGrowthRate(infSet.filter { case (q, _) => q == initState })._1
  }

  def trim: SST[State, InputAlph, OutputAlph] = {
    val transMap = trans.groupMap(_._1)(_._4)
    val reachables = Iterator
      .iterate(Set(initState))(ss => ss ++ ss.flatMap(transMap.getOrElse(_, Set())))
      .sliding(2)
      .find { case Seq(s1, s2) => s1 == s2 }
      .get
      .head
    println(reachables)
    this.copy(
      accepts = this.accepts & reachables,
      trans = this.trans.filter { case (s1, _, _, s2) =>
        reachables.contains(s1) && reachables.contains(s2)
      },
    )
  }
}

object SST {

  sealed trait VarOrAlphabet[+Alph]
  case class Var(name: String) extends VarOrAlphabet[Nothing] {
    override def toString(): String = name match {
      case "xout"   => "xₒ"
      case "xacc_0" => s"y${Visualizer.subscript(0)}"
      case "xacc_1" => s"y${Visualizer.subscript(1)}"
      case "xacc_2" => s"y${Visualizer.subscript(2)}"
      case "xacc_3" => s"y${Visualizer.subscript(3)}"
      case "xacc_4" => s"y${Visualizer.subscript(4)}"
      case "xacc_5" => s"y${Visualizer.subscript(5)}"
      case "xacc_6" => s"y${Visualizer.subscript(6)}"
      case "xacc_7" => s"y${Visualizer.subscript(7)}"
      case "xacc_8" => s"y${Visualizer.subscript(8)}"
      case "xacc_9" => s"y${Visualizer.subscript(9)}"
      case "xref_0" => s"z${Visualizer.subscript(0)}"
      case "xref_1" => s"z${Visualizer.subscript(1)}"
      case "xref_2" => s"z${Visualizer.subscript(2)}"
      case "xref_3" => s"z${Visualizer.subscript(3)}"
      case "xref_4" => s"z${Visualizer.subscript(4)}"
      case "xref_5" => s"z${Visualizer.subscript(5)}"
      case "xref_6" => s"z${Visualizer.subscript(6)}"
      case "xref_7" => s"z${Visualizer.subscript(7)}"
      case "xref_8" => s"z${Visualizer.subscript(8)}"
      case "xref_9" => s"z${Visualizer.subscript(9)}"
      case other    => other
    }
  }
  case class Alph[Alph](value: Alph) extends VarOrAlphabet[Alph] {
    override def toString(): String = value.toString()
  }

  class Update[Alph](val mapping: Map[Var, Seq[VarOrAlphabet[Alph]]]) {

    lazy val varDependency: Map[(Var, Var), Int] =
      mapping
        .flatMap { case (v1, form) =>
          form.collect { case v: Var => v }.map { v2 => (v1, v2) -> form.count(_ == v2) }
        }

    def varsDependsOn(vs: Set[Var]): Set[Var] =
      varDependency.collect {
        case ((asignee, depend), count) if (vs contains asignee) && count > 0 => depend
      }.toSet

    def pp: String =
      mapping.toSeq
        .sortBy(_._1.toString())
        .map { case (v, form) => s"$v := ${form.mkString}" }
        .mkString("\\n")
  }
  object Update {
    def apply[Alph](
        mapping: Map[Var, Seq[VarOrAlphabet[Alph]]],
        allVars: Set[Var]
    ): Update[Alph] = {
      val m = allVars.foldLeft(mapping) { case (mapping, v) =>
        mapping.updatedWith(v)(_ orElse Some(Seq(v: VarOrAlphabet[Alph])))
      }
      new Update(m)
    }
  }

  def referenceDSST(
      captIDs: Set[Int],
      domain: Set[Char]
  ): SST[Set[Int], ExtendedChar, Char] = {
    val xout = Var("xout")
    val xaccs = captIDs.map(i => i -> Var(s"xacc_$i")).toMap
    val xrefs = captIDs.map(i => i -> Var(s"xref_$i")).toMap
    val vars = xaccs.values.toSet ++ xrefs.values.toSet + xout
    val states = captIDs.subsets().toSet
    val wrappedDomain = domain.map(WrappedChar)
    val transChar =
      for (state <- states; in <- wrappedDomain)
        yield (
          state,
          in,
          Update[Char](
            ((xout -> Seq(xout, Alph(in.c))) +: state.toSeq.map(i =>
              xaccs(i) -> Seq(xaccs(i), Alph(in.c))
            )).toMap,
            vars
          ),
          state,
        )
    val transOpenParen =
      states.filter(_ != captIDs).flatMap { case state =>
        (captIDs -- state).map { id =>
          (state, OpenParen(id), Update[Char](Map(xaccs(id) -> Seq()), vars), state + id)
        }
      }
    val transCloseParen =
      states.filter(_.nonEmpty).flatMap { case state =>
        state.map { id =>
          (
            state,
            CloseParen(id),
            Update[Char](Map(xrefs(id) -> Seq(xaccs(id))), vars),
            state - id
          )
        }
      }
    val transRef =
      for (state <- states; id <- captIDs)
        yield (
          state,
          Reference(id),
          Update[Char](
            ((xout -> Seq(xout, xrefs(id))) +: state.toSeq.map(captID =>
              xaccs(captID) -> Seq(xaccs(captID), xaccs(id))
            )).toMap,
            vars
          ),
          state,
        )
    val trans = transChar ++ transOpenParen ++ transCloseParen ++ transRef
    SST(vars, xout, initState = Set(), accepts = Set(Set()), trans)
  }
}
