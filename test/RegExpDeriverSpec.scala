package matching.regexp

import org.scalatest._
import matching.monad._
import StateT._

class RegExpDeriverSpec extends FlatSpec with Matchers {
  implicit var deriver = new RegExpSetTreeDeriver()

  def applyLeaves[A](m: StateTBooleanSetTree[A])
    = m.apply(true)

  "derive" should "derive a" in {
    val r = RegExpParser("a")
    applyLeaves(r.derive('a')) should be (Set(Leaf(Some(EpsExp()), false)))
    applyLeaves(r.derive('b')) should be (Set(Fail))
  }
  it should "derive ab" in {
    val r = RegExpParser("ab")
    applyLeaves(r.derive('a')) should be (Set(Leaf(Some(ElemExp('b')), false)))
    applyLeaves(r.derive('b')) should be (Set(Fail))
    applyLeaves(r.derive('c')) should be (Set(Fail))
  }
  "derive star" should "derive a" in {
    val aster = RegExpParser("a*")
    applyLeaves(aster.derive('a')) should be (Set(Or(Leaf(Some(aster), false),Leaf(None, true))))
    applyLeaves(aster.derive('b')) should be (Set(Or(Fail,Leaf((None,true)))))
  }
  "derive star not-greedy" should "derive a" in {
    val aster = RegExpParser("a*?")
    applyLeaves(aster.derive('a')) should be (Set(Or(Leaf(None, true),Leaf(Some(aster), false))))
    applyLeaves(aster.derive('b')) should be (Set(Or(Leaf((None,true)),Fail)))
  }
  "derive MTree a" should "derive a" in {
    val a = RegExpParser("a")
    val r = MTreeExp(a)
    applyLeaves(r.derive('a')) should be (Set(Lft(Leaf((Some((MTreePrimeExp(EpsExp()))),false))),Fail))
    applyLeaves(r.derive('b')) should be (Set(Fail))
  }
  "derive MTree ." should "derive a" in {
    val dot = RegExpParser(".")
    val r = MTreeExp(dot)
    applyLeaves(r.derive('a')) should be (Set(Lft(Leaf((Some((MTreePrimeExp(EpsExp()))),false))),Fail))
    applyLeaves(r.derive('b')) should be (Set(Lft(Leaf((Some((MTreePrimeExp(EpsExp()))),false))),Fail))
  }
  "derive MTree ø" should "derive a" in {
    val r = MTreeExp(EmptyExp[Char])
    applyLeaves(r.derive('a')) should be (Set(Fail))
    applyLeaves(r.derive('b')) should be (Set(Fail))
  }
  "derive MTree aster" should "derive a" in {
    val aster = RegExpParser("a*")
    val r = MTreeExp(aster)
    applyLeaves(r.derive('a')) should be (Set(Lft(Leaf((Some((MTreePrimeExp(aster))),false))), Leaf((None,true)),Fail))
    applyLeaves(r.derive('b')) should be (Set(Fail,Leaf((None,true)),Fail))
  }
  it should "derive ." in {
    val r = RegExpParser(".")
    applyLeaves(r.derive('a')) should be (Set(Leaf(Some(EpsExp()), false)))
    applyLeaves(r.derive(None)) should be (Set(Leaf(Some(EpsExp()), false)))
    applyLeaves(r.derive('\n')) should be (Set(Fail))
  }

}
