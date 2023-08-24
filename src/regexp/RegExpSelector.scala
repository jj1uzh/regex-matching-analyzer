package matching.regexp

object RegExpSelector {

  private object RegExpTree {
    sealed trait RegExpTree {
      def name: String
      def src: RegExp[Char]
      def props: Map[String, String]

      def querySelectorAll(q: Query): Vector[RegExpTree] = {
        val v1 = if (q mch this) Vector(this) else Vector()
        val v2 = this match {
          case Leaf(_, _, _)          => Vector()
          case Node1(_, _, _, t1)     => t1.querySelectorAll(q)
          case Node2(_, _, _, t1, t2) => t1.querySelectorAll(q) ++ t2.querySelectorAll(q)
          case Node3(_, _, _, t1, t2, t3) =>
            t1.querySelectorAll(q) ++ t2.querySelectorAll(q) ++ t3.querySelectorAll(q)
        }
        v1 ++ v2
      }
    }

    // format: off
    case class Leaf(name: String, src: RegExp[Char], props: Map[String, String])
      extends RegExpTree

    case class Node1(name: String, src: RegExp[Char], props: Map[String, String],
                     t1: RegExpTree)
      extends RegExpTree

    case class Node2(name: String, src: RegExp[Char], props: Map[String, String],
                     t1: RegExpTree, t2: RegExpTree)
      extends RegExpTree

    case class Node3(name: String, src: RegExp[Char], props: Map[String, String],
                     t1: RegExpTree, t2: RegExpTree, t3: RegExpTree)
      extends RegExpTree
    // format: on

    def toTree(r: RegExp[Char]): RegExpTree =
      r match {
        case LookaheadExp(r1, true)   => Node1("?=", r, Map(), toTree(r1))
        case LookaheadExp(r1, false)  => Node1("?!", r, Map(), toTree(r1))
        case LookbehindExp(r1, true)  => Node1("?<=", r, Map(), toTree(r1))
        case LookbehindExp(r1, false) => Node1("?<!", r, Map(), toTree(r1))

        case BackReferenceExp(n, name) =>
          Leaf("br", r, Map("id" -> n.toString, "name" -> name.getOrElse("")))

        case GroupExp(r1, id, name, referenced, unique) =>
          val props =
            Array(
              Some("id" -> id.toString),
              name.map("name" -> _),
              referenced.map("ref" -> _.toString),
              unique.map("unique" -> _.toString)
            ).flatten.toMap
          Node1("capture", r, props, toTree(r1))

        case _: RegExpLeaf[_]      => Leaf("_", r, Map())
        case RegExpOp1(r1)         => Node1("_", r, Map(), toTree(r1))
        case RegExpOp2(r1, r2)     => Node2("_", r, Map(), toTree(r1), toTree(r2))
        case RegExpOp3(r1, r2, r3) => Node3("_", r, Map(), toTree(r1), toTree(r2), toTree(r3))
      }

    trait Query {
      def mch(r: RegExpTree): Boolean
      final def &(other: Query) = RegExpTree.&(this, other)
    }
    case class TypeQuery(name: String) extends Query {
      def mch(r: RegExpTree): Boolean = r.name == name
    }
    case class PropQuery(prop: String, value: String) extends Query {
      def mch(r: RegExpTree): Boolean = r.props.get(prop).contains(value)
    }
    case class &(q1: Query, q2: Query) extends Query {
      def mch(r: RegExpTree): Boolean = q1.mch(r) && q2.mch(r)
    }
    object Query {
      def compile(q: String): Query = {
        val pat = """([^\[]+)\[([^\[\]]+)\]""".r
        q match {
          case pat(name, props) =>
            props
              .split(",")
              .map { p =>
                val Array(k, v) = p.split("=").slice(0, 2)
                PropQuery(k, v)
              }
              .foldLeft[Query](TypeQuery(name))(_ & _)
          case _ => TypeQuery(q)
        }
      }
    }
  }

  def preproc(r: RegExp[Char]): RegExp[Char] =
    r /* TODO */

  implicit class RegExpQuerySelectorOps(val regexp: RegExp[Char]) extends AnyVal {

    def querySelectorAll(qstr: String): Seq[RegExp[Char]] =
      RegExpTree.toTree(regexp).querySelectorAll(RegExpTree.Query.compile(qstr)).map(_.src)

    def marked(qstr: String): RegExp[Char] =
      RegExpStructureAnalysis.markedAll(regexp, querySelectorAll(qstr))
  }
}
