package matching.monad

trait Monad[M[_]] {
  def unit[A](a: A): M[A]
  def bind[A,B](m: M[A], f: A => M[B]): M[B]
  def success[A]: M[A]
  def fail[A]: M[A]
  def concat[A](m1: M[A], m2: M[A]): M[A]
  def union[A](m1: M[A], m2: M[A]): M[A]//unionExpを扱うために暫定的に追加,MonadPlusをさらに拡張する必要がある？
  final def apply[A](a: A) = unit(a)
}

object Monad {
  implicit class MonadOp[M[_],A](self: M[A])(implicit m: Monad[M]) {
    def >>=[B](f: A => M[B]) = m.bind(self,f)
    def ++(m2: M[A]) = m.concat(self,m2)
  }

  implicit object ListMonad extends Monad[List] {
    def unit[A](a: A) = List(a)
    def bind[A,B](m: List[A], f: A => List[B]) = m.flatMap(f)
    def fail[A] = Nil
    def concat[A](m1: List[A], m2: List[A]) = m1 ++ m2
    def success[A] = Nil
    def union[A](m1: List[A], m2: List[A]) = m1 ++ m2
  }

  implicit object SetMonad extends Monad[Set] {
    def unit[A](a: A) = Set(a)
    def bind[A,B](m: Set[A], f: A => Set[B]) = m.flatMap(f)
    def fail[A] = Set()
    def concat[A](m1: Set[A], m2: Set[A]) = m1 ++ m2

    def success[A] = Set()
    def union[A](m1: Set[A], m2: Set[A]) = m1 ++ m2
  }
}
