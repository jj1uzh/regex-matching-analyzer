package matching.regexp

import matching.monad.Monad
import matching.monad.Monad.MonadOp
import matching.monad.StateOperatablenoAssert
import matching.monad.StateT
import matching.monad.Tree

object RegExpExec {

  private def run(
      r: RegExp[Char],
      w: String
  )(implicit
      mon: Monad[StateT.StateTMapTree],
      stateOp: StateOperatablenoAssert[StateT.StateTMapTree, Map[Int, String]]
  ): StateT.StateTMapTree[String] = r match {
    case EmptyExp()        => mon.fail
    case EpsExp()          => mon.unit(w)
    case ElemExp(c)        => if (w.headOption contains c) mon.unit(w.tail) else mon.fail
    case ConcatExp(r1, r2) => mon.bind(run(r1, w), run(r2, _: String))
    case AltExp(r1, r2)    => mon.concat(run(r1, w), run(r2, w))
    case star @ StarExp(r1, _) =>
      mon.concat(
        mon.bind(
          run(r1, w),
          { w2: String => if (w == w2) mon.unit(w2) else run(star, w2) }
        ),
        mon.unit(w)
      )
    case GroupExp(r1, id, _, _, _) =>
      run(r1, w) >>= { rest: String =>
        stateOp.update(_.updated(id, w.stripSuffix(rest))) >>= { _ => mon.unit(rest) }
      }
    case BackReferenceExp(id, _) =>
      stateOp.update(identity) >>= { state =>
        if (state contains id) {
          val tree = backrefCompare(state(id), w);
          { s =>
            Tree.TreeMonad.bind(tree, { a: String => Tree.TreeMonad.unit(a -> s) })
          }
        } else {
          mon.fail
        }
      }
  }

  private def backrefCompare(saved: String, w: String)(implicit
      mon: Monad[Tree]
  ): Tree[String] = saved match {
    case ""                                => mon.unit(w)
    case s if w.headOption contains s.head => backrefCompare(s.tail, w.tail) // TODO
    case _                                 => mon.fail
  }

  def exec(regex: RegExp[Char], str: String): Option[Map[Int, String]] = {
    run(regex, str).apply(Map.empty).find { case (w, _) => w.isEmpty }.map(_._2)
  }
}
