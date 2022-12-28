package matching.transition

import matching.Witness
import matching.monad._
import matching.tool.{Analysis, Debug}

import matching.monad.Monad._
import matching.monad.Tree._

class NonDetNoAssertTreeTransducer[Q,A](
  val states: Set[Q],
  val sigma: Set[A],
  val initialState: Q,
  val delta: Map[(Q,Option[A]),SetTree[Q]] // None: EOF ($)
) {
  def rename(): NonDetNoAssertTreeTransducer[Int,A] = {
    val renameMap = states.zipWithIndex.toMap
    val renamedStates = states.map(renameMap)
    val renamedInitialState = renameMap(initialState)
    val renamedDelta = delta.map{ case ((q,a),set) =>
      ((renameMap(q), a), set >>= (q => SetTreeMonad.unit(renameMap(q))))
    }

    new NonDetNoAssertTreeTransducer(renamedStates, sigma, renamedInitialState, renamedDelta)
  }

  //バックトラックを模倣するトランスデューサーに変換する
  def Bprune():NonDetNoAssertTreeTransducer[(Boolean,Option[Q]),A]={
      //tree内部の正規表現をf,aとの組に適切に置き換えた木を出力する
      //バックトラックの表現となる
      def hasLeaf(t: Tree[Q]): Boolean = {
        t match {
          case Leaf(a) => true
          case Success => false
          case Fail => false
          case Lft(l) => hasLeaf(l)
          case Or(l, r) => hasLeaf(l) || hasLeaf(r)
        }
      }
      def maketreeAF(t: Tree[Q]): SetTree[(Boolean,Option[Q])] = {
        t match{
          //どこかに必ずtrueが存在するとする
          case Fail => Set()
          case Success => Set(Success)
          case Leaf(a) => SetTreeMonad.unit((true,Some(a)))
          case Lft(t) => maketreeAF(t).flatMap(t => Set(Lft(t)))
          //左側にtrueがある場合(Lftをつける) ++ 右側にaがある場合
          case Or(t1,t2) =>  {
            if(hasLeaf(t1)) {
              maketreeAF(t1).flatMap(t => Set(Lft(t)):SetTree[(Boolean,Option[Q])]) union SetTreeMonad.concat(Set(maketreeF(t1)),maketreeAF(t2))
            }
            else SetTreeMonad.concat(Set(maketreeF(t1)),maketreeAF(t2))
          }
        }
      }

      def maketreeF(t: Tree[Q]): Tree[(Boolean,Option[Q])] ={
        //内部の状態を全てfalseに置き換える
        t match{
          case Fail => Fail
          case Success => Success
          case Leaf(a) => Leaf((false,Some(a)))
          case Lft(t) => Lft(maketreeF(t))
          case Or(t1,t2) => TreeMonad.concat(maketreeF(t1),maketreeF(t2))
        }
      }

    //新しい状態集合 true => 成功 false => 失敗
    val newstates:Set[(Boolean,Option[Q])] = Set((true,None)) ++ this.states.flatMap(r => Set((true,Some(r)),(false,Some(r))))
    val newsigma: Set[A] = this.sigma

    val newinitialState = (true,None)//ダミーの初期状態

    var newdelta = Map[((Boolean,Option[Q]),Option[A]),SetTree[(Boolean,Option[Q])]]()
    //ダミー文字により正しい初期状態に飛ぶ
    val none = this.sigma.toList.reverse.head//None
    newdelta += (((true,None),Some(none)) -> Set(Leaf((true,Some(this.initialState))),Leaf((false,Some(this.initialState)))))



    for(q <- this.states;a <- this.sigma){
        Analysis.checkInterrupted("preparing for calculate growth rate")
        if(this.delta.get((q,Some(a))) != None){
          //false_q ->^a t[f_1...f_n]
            newdelta += (((false,Some(q)),Some(a)) -> this.delta((q,Some(a))).flatMap(t => Set(maketreeF(t))))
          //true_q ->^a t[f_1,f_2,...a_i]
            newdelta += (((true,Some(q)),Some(a)) -> this.delta((q,Some(a))).flatMap(maketreeAF(_)))
        }
    }


    //$遷移を作る


    for(q <- this.states){
      Analysis.checkInterrupted("preparing for calculate growth rate")
        if(this.delta.get((q,None)) != None){
          //q \in F => a_q -> (q ->^$)  t(contains success)
          val destination_true = this.delta((q,None)).filter(hasSuccess(_))
          if(!destination_true.isEmpty)
          newdelta += (((true,Some(q)),None) -> destination_true.map(_>>= {case _ => Fail}))
          //q \not\in F=> f_q -> (q ->^$ ->) t(fail only)
          val destination_false = this.delta((q,None)).filterNot(hasSuccess(_))
          if(!destination_false.isEmpty)
          newdelta += (((false,Some(q)),None) -> destination_false.map(_>>= {case _ => Fail}))
          
        }
    }

    
    new NonDetNoAssertTreeTransducer[(Boolean,Option[Q]),A](newstates,newsigma,newinitialState,newdelta)
  }



  def toDeterministic() = {

    var newstates = Set[(Q,Int)]()
    val newinitialState = (this.initialState,1)
    //1文字もしくは，Map(Tuple)
    val newsigma = this.sigma.map(Left(_)) ++ this.states.map(Right(_))
    //遷移規則を作る
    var newdeltaDet = Map[((Q,Int),Option[Either[A,Q]]),Tree[(Q,Int)]]()
    var numoftrans_map = Map[Q,Int]()
    for(q<-this.states) numoftrans_map += (q -> this.delta.getOrElse((q,None),Set()).size)
    for(q <- this.states;a<-this.sigma){
      if(numoftrans_map(q) < this.delta.getOrElse((q,Some(a)),Set()).size){
        numoftrans_map += (q -> this.delta.getOrElse((q,Some(a)),Set()).size)
      }
    }

    for(q <- this.states){
      Analysis.checkInterrupted("preparing for calculate growth rate")
      val numoftrans = numoftrans_map(q)

      //状態，sigmaの構成
      for(i <- 1 to numoftrans){
        //状態と数の組へと変更する
        newstates = newstates ++ Set((q,i))
      }
      //遷移規則の構成
      for(a <- this.sigma;i <- 1 to numoftrans){
        Analysis.checkInterrupted("preparing for calculate growth rate")
      //一文字読む場合は状態と組になっているi番目の遷移を適用
        if(this.delta.get((q,Some(a))) != None){
          val n = this.delta((q,Some(a))).size
          if(i <= n){
            val destination = this.delta((q,Some(a))).toList(i-1) >>= {case q => Leaf((q,1))}
            newdeltaDet += (((q,i),Some(Left(a))) -> destination)
          }
          else if(n != 0){
            //用意されている遷移の数よりiが大きい時はn番目を適用
            val destination = this.delta((q,Some(a))).toList(n-1) >>= {case q => Leaf((q,1))}
            newdeltaDet += (((q,i),Some(Left(a))) -> destination)
          }
        }
      }
      for(i <- 1 to numoftrans-1){
        for(qprime <- this.states){
            val f = qprime
            if(q == qprime){
            //qの添字を1増やす
              newdeltaDet += (((q,i),Some(Right(f))) ->  TreeMonad.unit((q,i+1)))
            }
            else if(q != qprime){
              newdeltaDet += (((q,i),Some(Right(f))) ->  TreeMonad.unit((q,i)))
              }
          }
      }
      
      for(i <- 1 to numoftrans){
        //終端文字での遷移
        //状態と組になっているi番目の規則を適用
        val n = this.delta.getOrElse((q,None),Set()).size
        if(i <= n){
          //fail,success以外をfailに飛ばすことにより暫定的に型を合わせる
          newdeltaDet += (((q,i),None) -> (this.delta((q,None)).toList(i-1) >>= {case _ => Fail}))
        }

        else if(n != 0){
          //iが遷移規則の候補数よりも多い場合は最もindexが後ろのものに飛ばす
          newdeltaDet += (((q,i),None) -> (this.delta((q,None)).toList(n-1) >>= {case _ => Fail}))
        }
      }

      
    }
    Debug.info("Deterministic info") {
      ("number of states", newstates.size)
    }



    new DetNoAssertTreeTransducer[(Q,Int),Either[A,Q]](newstates,newsigma,newinitialState,newdeltaDet)
  }
  

  def calcGrowthRate() : (Option[Int], Witness[A]) = {
    val nfa_sigma:Set[Option[A]]= this.sigma.map(a => Some(a)) ++ Set(None)

    def forward() = {
      def add_trans (trans: Map[Set[Q], Set[Set[Q]]], src: Set[Q], dst: Set[Q]):
          Map[Set[Q], Set[Set[Q]]] = trans + (src -> (trans(src) + dst))

      def dfs(checkStates:List[Set[Q]], nfa_states: Set[Set[Q]],
        trans: Map[Set[Q], Set[Set[Q]]]):
          (Set[Set[Q]], Map[Set[Q], Set[Set[Q]]]) =
        checkStates match {
          case Nil => (nfa_states, trans)
          case qs::checkStates =>
            if (nfa_states(qs)) dfs(checkStates, nfa_states, trans)
            else {
              Analysis.checkInterrupted("construct indexedDT0L")
              val next_nfa_states = nfa_states + qs
              val (next_checkStates, next_trans) =
                nfa_sigma.foldLeft((checkStates, trans))
              { case ((checkStates, trans), a) =>
                val qqs =
                  qs.foldLeft(Set(Set[Q]())){ case (new_states, q) =>
                    this.delta.get((q,a)) match {
                      case None => Set()
                      case Some(ts) => ts.flatMap{ case t =>
                        val qs2 = flat(t).toSet
                        new_states.map(_ ++ qs2)
                      }
                    }
                  }
                (qqs.toList ++ checkStates,
                  qqs.foldLeft(trans){ case (trans, dst_qs) =>
                    add_trans(trans, dst_qs, qs) })
              }
              dfs(next_checkStates, next_nfa_states, next_trans)
            }
        }
      val init_trans = Map[Set[Q], Set[Set[Q]]]().withDefaultValue(Set())
      dfs(List(Set(this.initialState)), Set(), init_trans)
    }

    def backward(trans: Map[Set[Q], Set[Set[Q]]], start: Set[Q]): Set[Set[Q]] = { 
      def dfs(stack: List[Set[Q]], states: Set[Set[Q]]): Set[Set[Q]] =
        stack match {
          case Nil => states
          case qs::stack =>
            Analysis.checkInterrupted("construct indexedDT0L")
            if (states(qs)) dfs(stack, states)
            else
              dfs(trans(qs).toList ++ stack, states + qs)
        }
      dfs(List(start), Set())
    }

    type Morphs = Map[A, Set[Map[Q,Seq[Q]]]]
    type PreIDT0L = Map[(Set[Q], Set[Q]), Morphs]

    def add_morph(pIDT0L: PreIDT0L, src: Set[Q], dst:Set[Q],
      a: A, m: Map[Q,Seq[Q]]) : PreIDT0L = {
      val dT0L = pIDT0L((src, dst))
      val ms = dT0L.getOrElse(a, Set())
      pIDT0L + ((src, dst) -> (dT0L + (a -> (ms + m))))
    }

    def mkIndexedDT0L(states: Set[Set[Q]]): Map[(Set[Q], Set[Q]), NonDetDT0L[A,Q]] = {
      // DT0Ltrans: Set[Map[Q,Seq[Q]]]
      val init_preIDT0L0:PreIDT0L = Map[(Set[Q],Set[Q]),Morphs]()
      val init_preIDT0L = init_preIDT0L0.withDefaultValue(Map[A, Set[Map[Q,Seq[Q]]]]())
      val preIDT0L =
        states.foldLeft(init_preIDT0L){
          case (indexedDT0L, qs) =>
            Analysis.checkInterrupted("construct indexedDT0L")
            this.sigma.foldLeft(indexedDT0L){ case (indexedDT0L, a) =>
              val trans =
                qs.foldLeft(Set((Map[Q, Seq[Q]](), Set[Q]()))){
                  case (dT0L_trans, q) =>
                    this.delta.get((q,Some(a))) match {
                      case None => Set()
                      case Some(ts) => ts.flatMap{ case t =>
                        val qs2 = flat(t)
                        dT0L_trans.map{ case (m, dst_qs) =>
                          (m + (q -> qs2), dst_qs ++ qs2.toSet)
                        }
                      }
                    }
                }
              trans.foldLeft(indexedDT0L){ case (indexedDT0L, (m, dst_qs)) =>
                if (states(dst_qs))
                  add_morph(indexedDT0L, qs, dst_qs, a, m)
                else
                  indexedDT0L
              }
            }
        }
      preIDT0L.map{
        case ((src, dst),m) => ((src,dst), new NonDetDT0L(src, m))
      }
    }

    def toDT0LYM() = {
      val (qqs, trans) = forward()
      val qqs1 = backward(trans, Set())
      mkIndexedDT0L(qqs1)
    }
  
    def toDT0L(): Map[(Set[Q],Set[Q]),NonDetDT0L[A,Q]] = {
      //
      //H_{p,q}:q_i→t
      //NFA:2^Q {q_1,...q_n}→U(t|q_i→t)
      var checkStates:Set[Set[Q]] = Set(Set(this.initialState))
      //DT0LとNFAを同時に構成
      var nfa_states:Set[Set[Q]] = Set()
      val nfa_sigma:Set[Option[A]]= this.sigma.map(a => Some(a)) ++ Set(None)
      var nfa_delta: Seq[(Set[Q],Option[A],Set[Q])] = Seq()
      val nfa_initialStates = Set(Set(this.initialState))
      val nfa_finalStates:Set[Set[Q]] = Set(Set())//? None遷移が存在するstate

      //Set[Q]2つとDT0Lを対応させる
      var indexedDT0L = Map[(Set[Q],Set[Q]),NonDetDT0L[A,Q]]()

      while(!checkStates.isEmpty){
        Analysis.checkInterrupted("construct IndexedDT0L")
        var new_checkStates:Set[Set[Q]] = Set()
        for(statesSet <- checkStates){
          val dt0l_States = statesSet
          for(a <- this.sigma){
            
            //q_i→Seq(t)を全て集めたもの
            var dt0l_destination:Set[Map[Q,Seq[Q]]] = Set(Map())
            for(q <- statesSet){
              //NFA→{q_1,...q_n}→^a U{Set(t)|q_i→t}
              //H_{p,p'} =U_i q_i→Seq(t)
              var new_dt0l_destination:Set[Map[Q,Seq[Q]]] = Set()
              if(this.delta.get((q,Some(a))) != None){
                for(arrivetree <- this.delta((q,Some(a)))){
                  val stateseq = flat(arrivetree)
                  new_dt0l_destination ++= dt0l_destination.map(f => f + (q -> stateseq))
                }

              }
              dt0l_destination = new_dt0l_destination
            }
            //indexedDT0Lの構成
            //A→Set[Map[Q,Seq[Q]]]にしたい
            //同じp→p'となるようなdt0l_Morphsの組を集める
            var dt0l_Morphs: Map[A, Set[Map[Q,Seq[Q]]]] = Map()

            for(map <- dt0l_destination){
              var arrival:Set[Q] = Set()//p'
              

              for(q <- statesSet){
                if(map.get(q) != None) arrival ++= map(q).toSet
              }
              
              if(indexedDT0L.get((statesSet,arrival)) != None) dt0l_Morphs = indexedDT0L((statesSet,arrival)).morphs

              if(statesSet.forall(q => this.delta((q,Some(a))) != Set())){
                nfa_delta ++= Seq((statesSet,Some(a),arrival))
                if(!nfa_states.contains(arrival)){
                  nfa_states = nfa_states ++ Set(arrival)
                  new_checkStates = new_checkStates ++ Set(arrival)
                }
              }

              if(dt0l_Morphs.get(a) != None) dt0l_Morphs += (a -> (dt0l_Morphs(a)++Set(map)))
              else dt0l_Morphs += (a -> Set(map))

              val new_DT0L = new NonDetDT0L(dt0l_States,dt0l_Morphs)
              
              indexedDT0L += ((statesSet,arrival) -> new_DT0L)


              
            }
            
          }

          //NFAにおいてはEOFについても考える
          if(statesSet.forall(q => this.delta.get((q,None)) != None)){
            //EOF遷移のあとは状態がなくなる
            nfa_delta = nfa_delta ++ Seq((statesSet,None,Set[Q]()))
          }

        }
        //checkstatesの更新
        checkStates = new_checkStates

      }
      //finalstates

      var previousStates = Set[Set[Q]]()
      var reachStates:Set[Set[Q]] = Set(Set())
      while(previousStates != reachStates){
        previousStates = reachStates
          for(delta <- nfa_delta){
            delta match{
              case (qprime,a,q) => {
                if(reachStates.contains(q)) reachStates ++= Set(qprime)
            }
          }
        }
      }
      nfa_states = reachStates

        
      indexedDT0L.filterKeys({case (p,pprime) => nfa_states.contains(p) && nfa_states.contains(pprime)})

    }


    val dt0l= Debug.time("transducer -> DT0L") {
//      val indexedDT0L = toDT0L()
      val indexedDT0L = toDT0LYM()
      //indexedDT0Lから一つのNonDetDT0Lへと変換
      //ここをPairDT0Lへの変換にする
      var states = Set[(Q,Set[Q])]()
      var morphs = Map[(A,Int,Set[Q],Set[Q]),Map[(Q,Set[Q]),Seq[(Q,Set[Q])]]]()

      indexedDT0L.foreach{case ((p,pprime),indexdt0l) =>
        Analysis.checkInterrupted("construct DT0L")
        indexdt0l.states.foreach{case q =>
          states ++= Set((q,p),(q,pprime))
        }
        indexdt0l.morphs.foreach{case (a,map_set) =>
          Analysis.checkInterrupted("construct DT0L")
          val map_list = map_set.toList
          val n = map_list.size
          for(i <- 0 until n){
            val map = map_list(i)
            val map_ele = map.map{case (q,seq) => ((q,p)->seq.map((_,pprime)))}
            morphs += ((a,i,p,pprime) -> map_ele)
          }
        }
      }


      /*
      var morphs = Map[A,Set[Map[(Q,Set[Q]),Seq[(Q,Set[Q])]]]]()

      
      indexedDT0L.foreach{case ((p,pprime),indexdt0l) =>
        indexdt0l.states.foreach{case q =>
          states = states ++ Set((q,p),(q,pprime))
        }
        //morphsはa->Set[Map[Q,Seq[Q]]]に変わってる
        //map_set:Set[Map[Q,Seq[Q]]]
        indexdt0l.morphs.foreach{case (a,map_set) =>
          var f = Set[Map[(Q,Set[Q]),Seq[(Q,Set[Q])]]]()
          for(map <- map_set){
            var map_ele = Map[(Q,Set[Q]),Seq[(Q,Set[Q])]]()
            for((q,seq) <- map){
                map_ele = map_ele + ((q,p)->seq.map((_,pprime)))
            }
            f += map_ele
          }
          if(morphs.get(a) != None) morphs += (a ->(f ++ morphs(a)))
          else morphs = morphs + (a -> f)
        }

      }
      */


        new PairDT0L(states,morphs)
      }



    val (growthRate, witness, _) = dt0l.calcGrowthRate(Set((initialState,Set(initialState))))

  
    //witnessの最初のNoneを消す
    if(!witness.separators.isEmpty) {
      witness.separators = Seq(witness.separators.head.tail) ++ witness.separators.tail
      witness.separators :+= Seq() 
    }
    val adjusted_separators = witness.separators.map{case lst => lst.map{case (a,i,q,q_prime) => a}}
    val adjusted_pumps = witness.pumps.map{case lst => lst.map{case (a,i,q,q_prime) => a}}
    (growthRate.map(_+1), new Witness(adjusted_separators,adjusted_pumps))
  }


}
