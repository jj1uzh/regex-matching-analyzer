package matching.tool

import scala.collection.mutable.ArrayBuffer
import scala.sys.process._

object Command {
  case class CommandResult(exitCode: Int, out: ArrayBuffer[String], err: ArrayBuffer[String])

  def exec(command: String): CommandResult = {
    val out, err = ArrayBuffer[String]()
    val logger = ProcessLogger(out += _, err += _)
    val code = Process(command).!(logger)
    CommandResult(code, out, err)
  }
}
