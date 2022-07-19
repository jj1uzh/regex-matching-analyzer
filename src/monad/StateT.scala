package matching.monad

import AMonad._
import ATree._
import Tree._
import Monad._

trait StateOperatable[M[_,_], S] {
  def update[A](f: S => S): M[A,S]
}
trait StateOperatablenoAssert[M[_], S] {
  def update[A](f: S => S): M[S]
}

object StateT {
  type StateT[S,M[_,_],A,B] = S => M[(A,S),(B,S)]
  trait StateTBooleanATree[A,B] extends StateT[Boolean, ATree, A, B]

  type StateTnoAssert[S,M[_],A] = S => M[(A,S)]
  trait StateTBooleanSetTree[A] extends StateTnoAssert[Boolean, SetTree, A]
  trait StateTBooleanSet[A] extends StateTnoAssert[Boolean, Set, A]

  implicit object StateTATreeMonad extends AMonad[StateTBooleanATree] with StateOperatable[StateTBooleanATree, Boolean] {
    def unit[A,B](b: B) = s => ATreeMonad((b,s))
    def bindl[A,B,C](m: StateTBooleanATree[A,B], f: A => StateTBooleanATree[C,C])
      = s => m(s) `>>=l` {case (a,s) => f(a)(s)}
    def bindr[A,B,C](m: StateTBooleanATree[A,B], f: B => StateTBooleanATree[A,C])
      = s => m(s) `>>=r` {case (b,s) => f(b)(s)}
    def success[A,B] = _ => ATreeMonad.success
    def fail[A,B] = _ => ATreeMonad.fail
    def fail[A,B](m: StateTBooleanATree[A,B]) = s => ATreeMonad.fail(m(s))
    def plus[A,B](m1: StateTBooleanATree[A,B], m2: StateTBooleanATree[A,B])
      = s => m1(s) ++ m2(s)
    def assert[A,B](m1: StateTBooleanATree[A,A], m2: StateTBooleanATree[A,B])
      = s => ATreeMonad.assert(m1(s), m2(s))
    def assertNot[A,B](m1: StateTBooleanATree[A,A], m2: StateTBooleanATree[A,B])
      = s => ATreeMonad.assertNot(m1(s), m2(s))

    def update[A](f: Boolean => Boolean)
      = s => ATreeMonad((s, f(s)))


  }
  implicit object StateTSetTreeMonad extends Monad[StateTBooleanSetTree] with StateOperatablenoAssert[StateTBooleanSetTree,Boolean]{
    def unit[A](a: A) = s => SetTreeMonad((a,s))
    def bind[A,B](m: StateTBooleanSetTree[A], f: A => StateTBooleanSetTree[B])
      = s => SetTreeMonad.bind[(A,Boolean),(B,Boolean)](m(s),{case (a,s) => f(a)(s)})
    def fail[A] = _ => SetTreeMonad.fail
    def success[A] = _ => SetTreeMonad.success
    def concat[A](m1: StateTBooleanSetTree[A], m2: StateTBooleanSetTree[A])
      = s => SetTreeMonad.concat(m1(s),m2(s))

    def union[A](m1: StateTBooleanSetTree[A], m2: StateTBooleanSetTree[A])
      = s => (m1(s) union m2(s))

    def update[A](f: Boolean => Boolean)
      = s => SetTreeMonad.unit((s,f(s)))

    def pickLeaf[A](t: Tree[A]): SetTree[A]
      = t match{
          case Leaf(a) => Set(Leaf(a))
          case Or(t1,t2) => SetMonad.concat(pickLeaf(t1),pickLeaf(t2))
          case _ => Set.empty
        }

    def leaves[A](m: StateTBooleanSetTree[A]): StateTBooleanSetTree[A]
      = s => SetMonad.bind(m(s),pickLeaf)

    
    def putLft[A](t: Tree[A]): SetTree[A]
      = t match{
          case Leaf(a) => Set(Lft(Leaf(a)))
          case _ => Set.empty
        }
    def lft[A](m: StateTBooleanSetTree[A]): StateTBooleanSetTree[A]
      = s => SetMonad.bind(m(s),putLft)

  }
}
