package matching.transition


class NonDetDT0L[A,Q](
  val states: Set[Q],
  val morphs: Map[A, Set[Map[Q,Seq[Q]]]]
){


  def toDeterministic(): DT0L[(A,Int),Q] = {
    var det_morphs = Map[(A,Int), Map[Q,Seq[Q]]]()

    for((a,map_set) <- morphs){
      val len = map_set.size
      for(i <- 0 until len){
        det_morphs += ((a,i) -> map_set.toList(i))
      }
    }

    new DT0L(this.states,det_morphs)
  }
}
