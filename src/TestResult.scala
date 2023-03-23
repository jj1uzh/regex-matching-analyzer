package matching

sealed trait TestResult
case class Success(
  growthRate: Option[Int],
  witness: Witness[Char],
  approximated: Boolean,
  ruleSize: Int,
  time: Long
) extends TestResult {
  override def toString(): String = {
    val order =
      growthRate match {
        case Some(0) => "constant"
        case Some(1) => "linear"
        case Some(d) => s"polynomial, degree ${d}"
        case None    => s"exponential"
      }

    val b = new StringBuilder()
    b += '{'
    b ++= s"""\"order\": \"$order\", """
    b ++= s"""\"approx\": $approximated, """
    b ++= s"""\"witness\": ${if (witness.isEmpty) "null"
    else witness.toString}, """
    b ++= s"""\"time\": $time"""
    b += '}'
    b.result()
  }

  def getTime(): String = {
    s"${ruleSize}, ${time / 1000.0}"
  }
}

case class Skipped(message: String) extends TestResult {
  override def toString(): String = {
    s"skipped: ${message}"
  }
}

case class Error(message: String) extends TestResult {
  override def toString(): String = {
    s"error: ${message}"
  }
}

case object Timeout extends TestResult {
  override def toString(): String = {
    s"timeout"
  }
}
