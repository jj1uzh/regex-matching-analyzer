package matching.regexp

import matching.monad._
import AMonad._
import RegExp._
import Monad._
import StateT._

//SetTree ver.
class RegExpSetTreeDeriver(options: PCREOptions = new PCREOptions())(implicit m: Monad[StateTBooleanSetTree] with StateOperatablenoAssert[StateTBooleanSetTree,Boolean]){
  def derive[A](r: RegExp[A], a: Option[A]): StateTBooleanSetTree[Option[RegExp[A]]] = {
    def consume(r: RegExp[Char], a: Option[Char]): StateTBooleanSetTree[Option[RegExp[Char]]] = {
      def accept(a: Option[Char]): Boolean = {
        def acceptCharClass(r: CharClassElem, a: Option[Char]): Boolean = {
          r match {
            case SingleCharExp(c) => a match {
              case Some(a) =>
                if (options.ignoreCase && a.isLetter) {
                  a.toLower == c || a.toUpper == c
                } else a == c
              case None => false
            }
            case RangeExp(start, end) => a match {
              case Some(a) =>
                if (options.ignoreCase && a.isLetter) {
                  r.charSet.contains(a.toLower) || r.charSet.contains(a.toUpper)
                } else r.charSet.contains(a)
              case None => false
            }
            case r @ MetaCharExp(_) => a match {
              case Some(a) =>
                if (options.ignoreCase && a.isLetter) {
                  (r.charSet.contains(a.toLower) || r.charSet.contains(a.toUpper)) ^ r.negative
                } else r.charSet.contains(a) ^ r.negative
              case None => r.negative
            }
          }
        }

        r match {
          case ElemExp(b) => a match {
            case Some(a: Char) =>
              if (options.ignoreCase && a.isLetter) {
                a.toLower == b || a.toUpper == b
              } else {
                a == b
              }
            case None => false
          }
          case DotExp() => a match {
            case Some(a) => options.dotAll || a != '\n'
            case None => true
          }
          case CharClassExp(es,positive) => es.exists(acceptCharClass(_,a)) ^ !positive
          case r @ MetaCharExp(_) => acceptCharClass(r,a)
          case _ => throw new Exception(s"internal error.")
        }
      }
      if (accept(a)) m.update(_ => false) >>= (_ => m(Some(EpsExp()))) else m.fail
    }

    r match {
      case ElemExp(_) | DotExp() | CharClassExp(_,_) | MetaCharExp(_) => consume(r,a)
      case EmptyExp() => m.fail
      case EpsExp() => m(None)
      case ConcatExp(r1,r2) =>
        derive(r1,a) >>= {
          case Some(r) => m(Some(optConcatExp(r,r2)))
          case None => derive(r2,a)
        }
      case AltExp(r1,r2) =>
        derive(r1,a) ++ derive(r2,a)
      case StarExp(r1,greedy) =>
        val rd: StateTBooleanSetTree[Option[RegExp[A]]] = derive(r1,a) >>= {
          case Some(r2) => m(Some(optConcatExp(r2,r)))
          case None => m(None)
        }
        if (greedy ^ options.ungreedy) {
          rd ++ m(None)
        } else {
          (m(None): StateTBooleanSetTree[Option[RegExp[A]]]) ++ rd
        }
      case PlusExp(r,greedy) =>
        val rStar = StarExp(r,greedy)
        derive(r,a) >>= {
          case Some(r1) => m(Some(optConcatExp(r1,rStar)))
          case None => derive(rStar,a)
        }
      case OptionExp(r,greedy) =>
        val dr = derive(r,a)
        if (greedy ^ options.ungreedy) {
          dr ++ m(None)
        } else {
          (m(None): StateTBooleanSetTree[Option[RegExp[A]]]) ++ dr
        }
      case RepeatExp(r,min,max,greedy) =>
        val rDec = RepeatExp(r,min.map(_-1),max.map(_-1),greedy)
        val rd: StateTBooleanSetTree[Option[RegExp[A]]] = derive(r,a) >>= {
          case Some(r1) => m(Some(optConcatExp(r1,rDec)))
          case None => derive(rDec,a)
        }
        if (min.isDefined) {
          rd
        } else if (greedy ^ options.ungreedy) {
          rd ++ m(None)
        } else {
          (m(None): StateTBooleanSetTree[Option[RegExp[A]]]) ++ rd
        }
      case StartAnchorExp() => m.update(identity) >>= (b => if (b) m(None) else m.fail)
      case EndAnchorExp() => m.fail
      /*
      case LookaheadExp(r,positive) =>
        val rd = derive(r,a)
        if (positive) m.assert(rd, m(None)) else m.assertNot(rd, m(None))
        */
      case LookbehindExp(r,positive) => ???
      /*
      case FailEpsExp() => m.fail(m(None))
      */

      case MTreeExp(r) => {
        val lft_deriver = new RegExpSetLftDeriver()(StateTSetMTreeMonad)
        StateTSetMTreeMonad.concat(lft_deriver.derive(r,a) >>= {
          case None => m(None)
          case Some(r) => m(Some(MTreePrimeExp(r)))
        }
        ,StateTSetMTreeMonad.fail)
      }

      case MTreePrimeExp(r) => {
        val lft_deriver = new RegExpSetLftDeriver()(StateTSetMTreeMonad)
        lft_deriver.derive(r,a) >>= {
          case None => m(None)
          case Some(r) => m(Some(MTreePrimeExp(r)))
        }
      }

      case _ => throw new Exception(s"internal error.")
    }
  }

  def deriveEOL[A](r: RegExp[A]): StateTBooleanSetTree[Unit] = {
    r match {
      case ElemExp(_) | EmptyExp() | DotExp() | CharClassExp(_,_) | MetaCharExp(_) => m.fail
      case EpsExp() => m(())
      case ConcatExp(r1,r2) => deriveEOL(r1) >>= (_ => deriveEOL(r2))
      case AltExp(r1,r2) => deriveEOL(r1) ++ deriveEOL(r2)
      case StarExp(r,greedy) =>
        val rd = deriveEOL(r)
        if (greedy ^ options.ungreedy) {
          rd ++ m(())
        } else {
          m(()) ++ rd
        }
      case PlusExp(r,greedy) =>
        val rStar = StarExp(r,greedy)
        deriveEOL(r) >>= (_ => deriveEOL(rStar))
      case OptionExp(r,greedy) =>
        val dr = deriveEOL(r)
        if (greedy ^ options.ungreedy) {
          dr ++ m(())
        } else {
          m(()) ++ dr
        }
      case RepeatExp(r,min,max,greedy) =>
        val rDec = RepeatExp(r,min.map(_-1),max.map(_-1),greedy)
        val rd = deriveEOL(r) >>= (_ => deriveEOL(rDec))
        if (min.isDefined) {
          rd
        } else if (greedy ^ options.ungreedy) {
          rd ++ m(())
        } else {
          m(()) ++ rd
        }
      case StartAnchorExp() => m.update(identity) >>= (b => if (b) m(()) else m.fail)
      case EndAnchorExp() => m(())
      /*
      case LookaheadExp(r,positive) =>
        val rd = deriveEOL(r)
        if (positive) m.assert(rd, m(())) else m.assertNot(rd, m(()))
        */
      case LookbehindExp(r,positive) => ???
      /*
      case FailEpsExp() => m.fail(m(()))
      */

      case MTreeExp(r) => {
        val lft_deriver = new RegExpSetLftDeriver()(StateTSetMTreeMonad)
        StateTSetMTreeMonad.concat(
          lft_deriver.deriveEOL(r) >>= {_ => m(())}
        ,StateTSetMTreeMonad.fail)
      }

      case MTreePrimeExp(r) => {
        val lft_deriver = new RegExpSetLftDeriver()(StateTSetMTreeMonad)
        lft_deriver.deriveEOL(r) >>= {_ => m(())}
      }

      case _ => throw new Exception(s"internal error.")
    }
  }
}
