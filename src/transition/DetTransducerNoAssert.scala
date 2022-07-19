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
  
  //q_p(None)の追加
  override def calcGrowthRate(): (Option[Int], Witness[A]) = {
    def toDT0L(): DT0L[A,Option[Q]] = {
      //Noneとq_pが対応
      val dt0l_states = states.map(Option(_)) + None
      val morphs:Map[A,Map[Option[Q],Seq[Option[Q]]]] = sigma.map{ a =>
        Analysis.checkInterrupted("transducer -> DT0L")
        a -> (states.map( q =>{
          val t = leavesOfTree(deltaDet((q,Some(a)))).map(Option(_))
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


    //witness.separators :+= Seq()
    //witnessの最初のNoneを消す
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
  //Totalにする
  //Q -> (Q,Set[Q])
    def makeDFA(td: DetNoAssertTreeTransducer[Q,A]): DFA[Set[Q],A] = {
      //決定的トランスデューサーをDFAへと変換 サブセット構成
      var previousStates = Set[Set[Q]]()
      var reachStates = Set(Set(td.initialState))
      //現時点での状態から進める状態をマークしていき，なくなるまで続ける
      while(previousStates != reachStates){
        Analysis.checkInterrupted("preparing for calculate growth rate")
        val checkStates = reachStates diff previousStates
        previousStates = reachStates
        for(states <- checkStates;a <- td.sigma){
          if(states.forall(q => td.deltaDet.get((q,Some(a))) != None)){
            reachStates ++= Set(states.flatMap(q => leavesOfTree(td.deltaDet((q,Some(a)))).toSet))
          }
        }
      }
      val states = reachStates//Reachableなものに制限
      val sigma = td.sigma
      val initial = Set(td.initialState)
      //$遷移が存在する状態しか含んでいない集合全体が受理状態（空集合除く？）
      val statesCanEOFtrans = td.states.filter(q => td.deltaDet.get((q,None)) != None)
      val finalstates = states.filter(q => q.subsetOf(statesCanEOFtrans))//EOF遷移がある状態しか含んでいない集合
      var newdelta = Map[(Set[Q],A),Set[Q]]()
      for(setq <- states;a <- td.sigma){
          Analysis.checkInterrupted("preparing for calculate growth rate")
          if(setq.forall(q => td.deltaDet.get((q,Some(a))) != None)){
          //状態の集合に対しtdの遷移が存在する場合 遷移先の状態を全て集めてそこに飛ばす
            val destination = setq.flatMap(q => leavesOfTree(td.deltaDet((q,Some(a)))).toSet)
            newdelta += ((setq,a) -> destination)
          }
      }
      new DFA(states,sigma,newdelta,initial,finalstates)
    }

    def canAcceptStates(dfa: DFA[Set[Q],A])= {
      //受理可能な状態を返す
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
    //上で構成したDFAを利用してdomainの判定を行う
    val dfa = makeDFA(this)
    var newstates = Set[(Q,Set[Q])]()
    //q \in S かつ dom_S(td) ≠ \emptyset かつ あるwに対してSはq_0からwで遷移したときに登場するstate
    val usefulstate = canAcceptStates(dfa)  //dom_S(td) ≠ \emptyset となるS
    for(stateset <- usefulstate){
      //stateの構成 statesetはReachableかつacceptableなものに限る
      for(q <- stateset) newstates = newstates ++ Set((q,stateset))
    }


    val newsigma = this.sigma
    val newinitialState = (this.initialState,Set(this.initialState))
    var newdeltaDet = Map[((Q,Set[Q]),Option[A]),Tree[(Q,Set[Q])]]()
      //任意のwについてaw\notin dom_S(td)の時constantに飛ばす
    for((q,s) <- newstates;a <- this.sigma){
      Analysis.checkInterrupted("preparing for calculate growth rate")
      //DFAについてaで遷移した先の状態からAcceptableかを調査する
      //↑がaw \notin dom_S(td)の判定となる
      if(this.deltaDet.get((q,Some(a))) != None && dfa.deltaDet.get((s,a)) != None && (usefulstate.contains(dfa.deltaDet((s,a))))){
        //遷移先は\delta_S(t)
        newdeltaDet += (((q,s),Some(a)) -> (this.deltaDet((q,Some(a))) >>= {case q => TreeMonad.unit((q,dfa.deltaDet((s,a))))}))
      }
      else{
        //constant(Fail)に飛ばす
        newdeltaDet += (((q,s),Some(a)) -> Fail)
      }
    }
    for((q,s) <- newstates){
      Analysis.checkInterrupted("preparing for calculate growth rate")
      if(this.deltaDet.get((q,None)) == None){
        //constant(Fail)に飛ばす
        newdeltaDet += (((q,s),None) -> Fail)
      }
      else{
        //通常通り 木にはsuccessとfailしかない？暫定的に葉をFailにさせることで型を一致
        newdeltaDet += (((q,s),None) -> (this.deltaDet((q,None)) >>= {case _ => Fail}))

      }
    }
    Debug.info("Totalize info") {
      ("number of states", newstates.size)
    }


    new DetNoAssertTreeTransducer[(Q,Set[Q]),A](newstates,newsigma,newinitialState,newdeltaDet)
}

}
