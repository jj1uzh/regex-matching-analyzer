package matching.transition

import collection.mutable.Stack
import matching.Witness
import matching.monad._
import matching.tool.{Analysis, Debug}
import matching.monad.Monad._
import matching.monad.Tree._

class DetNoAssertTreeTransducer[Q,A](
  states: Set[Q],
  sigma: Set[A],
  initialState: Q,
  val deltaDet: Map[(Q,Option[A]),Tree[Q]] // None: EOF ($)
)extends NonDetNoAssertTreeTransducer(
  states,
  sigma,
  initialState,
  deltaDet.map{ case ((q1,a),t) =>
    Analysis.checkInterrupted("construct transducer")
    ((q1,a),Set(t))
  }
){
  override def rename(): DetNoAssertTreeTransducer[Int,A] = {
    val renameMap = states.zipWithIndex.toMap
    val renamedStates = states.map(renameMap)
    val renamedInitialState = renameMap(initialState)
    val renamedDelta = deltaDet.map{ case ((q,a), t) =>
      (renameMap(q),a) -> (t >>= (q => Leaf[Int](renameMap(q))))
    }
    new DetNoAssertTreeTransducer(renamedStates, sigma, renamedInitialState, renamedDelta)
  }
  
  override def calcGrowthRate(): (Option[Int], Witness[A]) = {
    def toDT0L(): DT0L[A,Option[Q]] = {
      val dt0l_states = states.map(Option(_)) + None
      val morphs:Map[A,Map[Option[Q],Seq[Option[Q]]]] = sigma.map{ a =>
        Analysis.checkInterrupted("transducer -> DT0L")
        a -> (states.map( q =>{
          val t = flat(deltaDet((q,Some(a)))).map(Option(_))
          if(isNonTrivial(deltaDet((q,Some(a))))) (Option(q) -> (t:+None)) 
          else (Option(q) -> t)
        }
        ).toMap + (None -> Seq(None)))
      }.toMap

      new DT0L(dt0l_states, morphs)
    }

    val dt0l = Debug.time("transducer -> DT0L") {
      toDT0L()
    }


    val (growthRate, witness, _) = dt0l.calcGrowthRate(Option(initialState))


    //erase first dummy char
    if(!witness.separators.isEmpty) witness.separators = Seq(witness.separators.head.tail) ++ witness.separators.tail

    (growthRate, witness)
  }
  

  def toReverseDFA(): DFA[Set[Q], A] = {
    val newInitial = states.filter(q => TreeMonad.eval(deltaDet((q,None)))(_ => true))
    var newStates = Set(newInitial)
    val stack = Stack(newInitial)
    var newDelta = Map[(Set[Q],A), Set[Q]]()

    while (stack.nonEmpty) {
      Analysis.checkInterrupted("transducer -> DFA")
      val qs = stack.pop
      sigma.foreach{a =>
        val next = states.filter(q => TreeMonad.eval(deltaDet((q,Some(a))))(qs))
        newDelta += (qs,a) -> next
        if (!newStates.contains(next)) {
          newStates += next
          stack.push(next)
        }
      }
    }

    val newFinal = newStates.filter(_.contains(initialState))
    new DFA(newStates, sigma, newDelta, newInitial, newFinal)
  }



def toTotal():DetNoAssertTreeTransducer[(Q,Set[Q]),A] = {
  //state:Q -> (Q,Set[Q])
    def makeDFA(td: DetNoAssertTreeTransducer[Q,A]): DFA[Set[Q],A] = {
      //subset construction

      //restrict reachable states
      var previousStates = Set[Set[Q]]()
      var reachStates = Set(Set(td.initialState))
      while(previousStates != reachStates){
        Analysis.checkInterrupted("preparing for calculate growth rate")
        val checkStates = reachStates diff previousStates
        previousStates = reachStates
        for(states <- checkStates;a <- td.sigma){
          if(states.forall(q => td.deltaDet.get((q,Some(a))) != None)){
            reachStates ++= Set(states.flatMap(q => flat(td.deltaDet((q,Some(a)))).toSet))
          }
        }
      }

      val dfa_states = reachStates
      val dfa_sigma = td.sigma
      val dfa_initial = Set(td.initialState)
      val statesCanEOFtrans = td.states.filter(q => td.deltaDet.get((q,None)) != None)
      val dfa_finalstates = dfa_states.filter(q => q.subsetOf(statesCanEOFtrans))//only sets contains EOF trans
      var dfa_delta = Map[(Set[Q],A),Set[Q]]()
      for(state <- dfa_states;a <- td.sigma){
        //construct transition
          Analysis.checkInterrupted("preparing for calculate growth rate")
          if(state.forall(q => td.deltaDet.get((q,Some(a))) != None)){
            //collect states in tree
            val destination = state.flatMap(q => flat(td.deltaDet((q,Some(a)))).toSet)
            dfa_delta += ((state,a) -> destination)
          }
      }
      new DFA(dfa_states,dfa_sigma,dfa_delta,dfa_initial,dfa_finalstates)
    }

    def acceptableStates(dfa: DFA[Set[Q],A])= {
      //return acceptable states
      var previousStates = Set[Set[Q]]()
      var reachStates = dfa.finalStates
      while(previousStates != reachStates){
        Analysis.checkInterrupted("preparing for calculate growth rate")
        previousStates = reachStates
        for(q <- dfa.states diff reachStates;a <- dfa.sigma){
          if(dfa.deltaDet.get((q,a)) != None && reachStates.contains(dfa.deltaDet((q,a)))){
            reachStates ++= Set(q)
          }
        }
      }
      reachStates
    }

    val dfa = makeDFA(this)
    var newstates = Set[(Q,Set[Q])]()
    val usefulstate = acceptableStates(dfa)
    for(state <- usefulstate){
      //stateset:reachable and acceptable
      for(q <- state) newstates ++= Set((q,state))//q\in state
    }


    val newsigma = this.sigma
    val newinitialState = (this.initialState,Set(this.initialState))
    var newdeltaDet = Map[((Q,Set[Q]),Option[A]),Tree[(Q,Set[Q])]]()
    for((q,s) <- newstates;a <- this.sigma){
      Analysis.checkInterrupted("preparing for calculate growth rate")
      if(this.deltaDet.get((q,Some(a))) != None && dfa.deltaDet.get((s,a)) != None && (usefulstate.contains(dfa.deltaDet((s,a))))){
        //delta(q,a) is acceptable in DFA
        newdeltaDet += (((q,s),Some(a)) -> (this.deltaDet((q,Some(a))) >>= {case q => TreeMonad.unit((q,dfa.deltaDet((s,a))))}))
      }
      else{
        //delta(q,a) is not acceptable in DFA
        //to constant(Fail)
        newdeltaDet += (((q,s),Some(a)) -> Fail)
      }
    }
    for((q,s) <- newstates){
      Analysis.checkInterrupted("preparing for calculate growth rate")
      if(this.deltaDet.get((q,None)) == None){
        //to constant(Fail)
        newdeltaDet += (((q,s),None) -> Fail)
      }
      else{
        newdeltaDet += (((q,s),None) -> (this.deltaDet((q,None)) >>= {case _ => Fail}))

      }
    }
    Debug.info("Totalize info") {
      ("number of states", newstates.size)
    }


    new DetNoAssertTreeTransducer[(Q,Set[Q]),A](newstates,newsigma,newinitialState,newdeltaDet)
}

}
