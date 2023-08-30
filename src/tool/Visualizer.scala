package matching.tool

import matching.transition.Graph
import matching.transition.NFA
import matching.transition.SST

object Visualizer {

  def subscript(n: Int): String = n match {
    case m if m < 0   => "₋" + subscript(-n)
    case m if m >= 10 => subscript(n / 10) + subscript(n % 10)
    case 0            => "₀"
    case 1            => "₁"
    case 2            => "₂"
    case 3            => "₃"
    case 4            => "₄"
    case 5            => "₅"
    case 6            => "₆"
    case 7            => "₇"
    case 8            => "₈"
    case 9            => "₉"
  }

  def esc(a: Any): String = {
    val aa = a match {
      case s: Set[_]      => s"{${s.mkString(",")}}"
      case (s: Set[_], t) => s"{${s.mkString(",")}}, $t"
      case other          => other
    }
    aa.toString.map {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case c    => c
    }.mkString
  }

  implicit class GraphVisualizer[V](g: Graph[V]) {
    protected def visualizeNodes(file: File, renameMap: Map[V, Int]): Unit = {
      g.nodes.foreach(v => file.writeln(s""""${renameMap(v)}" [label = "${esc(v)}"];""", 1))
      file.writeln()
    }

    protected def visualizeEdges(file: File, renameMap: Map[V, Int]): Unit = {
      g.edges.foreach { case (v1, v2) =>
        file.writeln(s""""${renameMap(v1)}" -> "${renameMap(v2)}";""", 1)
      }
    }

    def visualize(name: String): Unit = {
      val file = IO.createFile(s"${name}.dot", true)
      val renameMap = g.nodes.zipWithIndex.toMap

      file.writeln(s"digraph ${name.replace("/", "_")} {")

      file.writeln("graph [", 1)
      file.writeln("rankdir = LR", 2)
      file.writeln("];", 1)
      file.writeln()

      file.writeln("node [", 1)
      file.writeln("shape = circle", 2)
      file.writeln("];", 1)
      file.writeln()

      visualizeNodes(file, renameMap)
      file.writeln()

      visualizeEdges(file, renameMap)
      file.writeln()

      file.writeln("}")

      file.close()

      Command.exec(s"dot -T pdf ${name}.dot -o ${name}.pdf")
    }
  }

  implicit class NFAVisualizer[Q, A](nfa: NFA[Q, A]) extends GraphVisualizer[Q](nfa) {
    override protected def visualizeNodes(file: File, renameMap: Map[Q, Int]): Unit = {
      file.writeln("\"initial\" [", 1)
      file.writeln("label = \"\",", 2)
      file.writeln("shape = none,", 2)
      file.writeln("fixedsize = true,", 2)
      file.writeln("width = 0,", 2)
      file.writeln("height = 0", 2)
      file.writeln("];", 1)

      nfa.states.foreach { state =>
        if (nfa.finalStates.contains(state)) {
          file.writeln(
            s""""${renameMap(state)}" [label = "${esc(state)}", shape = doublecircle];""",
            1
          )
        } else {
          file.writeln(s""""${renameMap(state)}" [label = "${esc(state)}"];""", 1)
        }
      }
    }

    override protected def visualizeEdges(file: File, renameMap: Map[Q, Int]): Unit = {
      nfa.initialStates.foreach { initialState =>
        file.writeln(s""""initial" -> "${renameMap(initialState)}";""", 1)
      }

      nfa.delta.foreach { case (q1, a, q2) =>
        file.writeln(s""""${renameMap(q1)}" -> "${renameMap(q2)}" [label = "${esc(a)}"];""", 1)
      }
    }
  }

  case class SSTVisualizer[Q, A, B](sst: SST[Q, A, B]) extends AnyVal {
    private def visualizeNodes(file: File, renameMap: Map[Q, Int]): Unit = {
      file.writeln("\"initial\" [", 1)
      file.writeln("label = \"\",", 2)
      file.writeln("shape = none,", 2)
      file.writeln("fixedsize = true,", 2)
      file.writeln("width = 0,", 2)
      file.writeln("height = 0", 2)
      file.writeln("];", 1)

      sst.states.foreach { state =>
        if (sst.accepts.contains(state)) {
          file.writeln(
            s""""${renameMap(state)}" [label = "${esc(state)}", shape = doublecircle];""",
            1
          )
        } else {
          file.writeln(s""""${renameMap(state)}" [label = "${esc(state)}"];""", 1)
        }
      }
    }

    private def visualizeEdges(file: File, renameMap: Map[Q, Int]): Unit = {
      file.writeln(s""""initial" -> "${renameMap(sst.initState)}";""", 1)

      sst.trans.foreach { case (q1, a, update, q2) =>
        file.writeln(
          s""""${renameMap(q1)}" -> "${renameMap(q2)}" [label = "${esc(a)}\n${update.pp}"];""",
          1
        )
      }
    }

    def visualize(name: String): Unit = {
      val file = IO.createFile(s"${name}.dot", true)
      val renameMap = sst.states.zipWithIndex.toMap
      file.writeln(s"digraph ${name.replace("/", "_")} {")
      file.writeln("graph [", 1)
      file.writeln("rankdir = LR", 2)
      file.writeln("];\n", 1)
      file.writeln("node [", 1)
      file.writeln("shape = circle", 2)
      file.writeln("];\n", 1)
      visualizeNodes(file, renameMap)
      file.writeln()
      visualizeEdges(file, renameMap)
      file.writeln()
      file.writeln("}")
      file.close()
      Command.exec(s"dot -T pdf ${name}.dot -o ${name}.pdf")
    }
  }
}
