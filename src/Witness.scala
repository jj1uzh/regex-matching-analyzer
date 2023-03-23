package matching

import tool.IO

case class Witness[A](var separators: Seq[Seq[A]],pumps: Seq[Seq[A]]) {
  override def toString(): String = {
    val escapedSeparators = separators.map(_.map(IO.escape).mkString)
    val escapedPumps =
      pumps.map(_.map(IO.escape).mkString("", "", ""))

    (escapedSeparators.head +: (escapedPumps zip escapedSeparators.tail).map { case (a, b) =>
      Seq(a, b)
    }.flatten)
      .map(a => s""""${a}\"""")
      .mkString("[", ",", "]")
  }

  def isEmpty: Boolean =
    this == Witness.empty
}

object Witness {
  def empty[A]: Witness[A] = Witness(Seq(),Seq())
}
