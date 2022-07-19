import org.scalatest._

import matching.regexp._
import matching.monad._
import matching.transition._

class stringtotreeTest extends FlatSpec {
  /*
  "toDeterministic " should "success" in {

  val states1 = Set(1,2,3)
  val sigma1 = Set('a','b')
  val initialState = 1

  var transition = Map[(Int,Option[Char]),Set[Tree[Int]]]()
  transition = transition + ((1,Some('a')) -> Set(Leaf(1),Leaf(2),Leaf(3)))

  for(q <- states1){
    transition = transition + ((q,None) -> Set(Success))
  }

  val td1 = new NonDetNoAssertTreeTransducer(states1,sigma1,initialState,transition)

  val td2 = td1.toDeterministic()

  assert(td2.states == Set((1,1),(1,2),(1,3),(2,1),(3,1)))
  assert(td2.deltaDet(((1,1),Some(Left('a')))) == Leaf((1,1)) && td2.deltaDet(((1,2),Some(Left('a')))) == Leaf((2,1)) && td2.deltaDet(((1,3),(Some(Left('a'))))) == Leaf((3,1)))
  assert(td2.deltaDet(((1,1),None)) == Success)

  }
  "toTotal" should "success" in {

  val states1 = Set(1,2,3)
  val sigma1 = Set('a','b')
  val initialState = 1

  var transition = Map[(Int,Option[Char]),Tree[Int]]()
  transition = transition + ((1,Some('a')) -> Or(Leaf(2),Leaf(3)))

  transition = transition + ((2,None) -> Success)
  transition = transition + ((3,None) -> Success)

  val td1 = new DetNoAssertTreeTransducer(states1,sigma1,initialState,transition)

  val td2 = td1.toTotal()
  //td2.states = Set((2,Set(2, 3)), (3,Set(2, 3)), (1,Set(1)))
  assert(td2.states.contains((2,Set(2,3))))
  assert(td2.deltaDet(((1,Set(1)),None)) == Fail)
  assert(td2.deltaDet(((2,Set(2,3)),None)) == Success)
  assert(td2.deltaDet(((1,Set(1)),Some('a'))) == Or(Leaf((2,Set(2,3))),Leaf((3,Set(2,3)))))
  }
  */
  /*
  "Bprune" should "success" in {

    val states1 = Set(1,2,3)
    val sigma1 = Set('a','b')
    val initialState = 1

    var transition = Map[(Int,Option[Char]),Set[Tree[Int]]]()
    transition += ((1,Some('a')) -> Set(Or(Leaf(1),Leaf(2)),Leaf(3)))


    transition += ((1,None) -> Set(Success,Or(Success,Fail)))
    transition += ((3,None) -> Set(Fail,Or(Success,Fail)))


    val td1 = new NonDetNoAssertTreeTransducer(states1,sigma1,initialState,transition)

    val td2 = td1.Bprune()
    assert(td2.delta(((true,None),Some(None))) == Set(Leaf((true,Some(1))),Leaf((false,Some(1)))))
    assert(td2.delta.get(((true,None),None)) == None)
    assert(td2.delta(((true,Some(1)),Some(Some('a')))) == Set(Lft(Leaf((true,Some(1)))),Or(Leaf((false,Some(1))),Leaf((true,Some(2)))), Leaf((true,Some(3)))))
    assert(td2.delta(((true,Some(1)),None)) == Set(Or(Success,Fail),Success))
    assert(td2.delta(((false,Some(3)),None)) == Set(Fail))

  }
  */

  





}
