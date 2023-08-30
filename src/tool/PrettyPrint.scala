package matching.tool

trait PrettyPrint {
  this: Product =>

  private def toPrettyStringSingle(obj: Any, level: Int): String = {
    obj match {
      case m: Map[_, _] =>
        m.map { case (k, v) => s"$k -> $v" }
          .mkString(
            "Map(\n" + "  " * (1 + level),
            ",\n" + "  " * (level + 1),
            "\n" + "  " * level + ")"
          )
      case s: Set[_] =>
        s.mkString(
          "Set(\n" + "  " * (level + 1),
          ",\n" + "  " * (level + 1),
          "\n" + "  " * level + ")"
        )
//      case p: Product   => toPrettyStringProduct(p, level)
      case other => other.toString
    }
  }

  private def toPrettyStringProduct(p: Product, level: Int): String = {
    Seq(
      Seq(s"${p.productPrefix}("),
      (0 until productArity).map { i =>
        "  " + s"${productElementName(i)} = ${toPrettyStringSingle(productElement(i), level + 2)}"
      //      "  " + s"${productElementName(i)} = ${productElement(i)}"
      },
      Seq(")"),
    ).flatten.mkString("  " * level, ",\n" + "  " * level, "")
  }

  def toPrettyString: String =
    toPrettyStringProduct(this, 0)
}
