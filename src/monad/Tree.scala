package matching.monad

import Monad._

sealed trait Tree[+A]
case class Leaf[A](a: A) extends Tree[A]
case object Success extends Tree[Nothing]
case object Fail extends Tree[Nothing]
case class Or[A](l: Tree[A], r: Tree[A]) extends Tree[A]

sealed trait CutTree[+A] extends Tree[A]
case class Lft[A](l: Tree[A]) extends CutTree[A]

object Tree {
  implicit object TreeMonad extends Monad[Tree] {
    def unit[A](a: A) = Leaf(a)
    def bind[A,B](m: Tree[A], f: A => Tree[B]) = {
      m match {
        case Leaf(a) => f(a)
        case Success => Success
        case Fail => Fail
        case Or(l,r) => Or(l >>= f, r >>= f)
        case Lft(l) => Lft(l >>= f)
      }
    }
    def fail[A] = Fail
    def concat[A](m1: Tree[A], m2: Tree[A]) = Or(m1,m2)
    def eval[A](m: Tree[A])(v: A => Boolean): Boolean = {
      m match {
        case Leaf(a) => v(a)
        case Success => true
        case Fail => false
        case Or(l,r) => eval(l)(v) || eval(r)(v)
        case Lft(_) => throw new Exception("eval operetor is undefined for Lft.")
      }
    }
    def success[A] = Success

  }

  type SetTree[A] = Set[Tree[A]]

  object SetMTreeMonad extends Monad[SetTree]{
    //Set of Monadic Tree
    def unit[A](a: A) = Set(Leaf(a))
    def bind[A,B](a: SetTree[A], f:A => SetTree[B]): SetTree[B] = {
      def bind0(t: Tree[A], f:A => SetTree[B]): SetTree[B] = {
        t match{
          case Leaf(a) => f(a)
          case Success => Set(Success)
          case Fail => Set(Fail)
          case Lft(l) => bind0(l,f).flatMap(t => Set(Lft(t)))
          case Or(t1,t2) => SetTreeMonad.concat(bind0(t1,f),bind0(t2,f))
        }
      }
      a.flatMap(t => bind0(t,f))
    }
    def success[A] = Set(Success)
    def fail[A] = Set(Fail)
    def concat[A](m1: SetTree[A], m2: SetTree[A]) = m1 union m2//different from SetTree

  }

  implicit object SetTreeMonad extends Monad[SetTree]{
    def unit[A](a: A) = Set(Leaf(a))
    def bind[A,B](a: SetTree[A], f:A => SetTree[B]): SetTree[B] = {
      def bind0(t: Tree[A], f:A => SetTree[B]): SetTree[B] = {
        t match{
          case Leaf(a) => f(a)
          case Success => Set(Success)
          case Fail => Set(Fail)
          case Lft(l) => bind0(l,f).flatMap(t => Set(Lft(t)))
          case Or(t1,t2) => SetTreeMonad.concat(bind0(t1,f),bind0(t2,f))
        }
      }
      a.flatMap(t => bind0(t,f))
    }
    def success[A] = Set(Success)
    def fail[A] = Set(Fail)
    def concat[A](m1: SetTree[A], m2: SetTree[A]) = m1.flatMap(tree1 => m2.flatMap(tree2 => Set(Or(tree1,tree2))))

  }

  def flat[Q](m: Tree[Q]): Seq[Q] = {
    m match {
      case Leaf(q) => Seq(q)
      case Success | Fail => Seq()
      case Or(l,r) => flat(l) ++ flat(r)
      case Lft(l) => flat(l)
    }
  }

  def hasSuccess[Q](t: Tree[Q], qs: Set[Q] = Set[Q]()): Boolean = {
    t match {
      case Leaf(q) => qs.contains(q)
      case Success => true
      case Fail => false
      case Or(l,r) => hasSuccess(l, qs) || hasSuccess(r, qs)
      case Lft(l) => hasSuccess(l, qs)
    }
  }

  def cut[Q](t: Tree[Q], qs: Set[Q] = Set[Q]()): Tree[Q] = {
    t match {
      case Or(l,r) => if (hasSuccess(l,qs)) Lft(cut(l,qs)) else Or(l,cut(r,qs))
      case _ => t
    }
  }

  def isNonTrivial[A](m: Tree[A]): Boolean = {
    m match {
      case Leaf(a) => false
      case _ => true
    }
  }

  def pruneNoAssert[Q](t: Tree[Q], qs: Set[Q] = Set[Q]()): Tree[Q] = {
    t match {
      case Or(l,r) => if (TreeMonad.eval(l)(qs)) Lft(pruneNoAssert(l,qs)) else Or(l,pruneNoAssert(r,qs))
      case _ => t
    }
  }
}
