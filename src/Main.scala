package matching

import enumeratum.EnumEntry
import matching.TestResult._
import matching.regexp.RegExp._
import matching.regexp.RegExpStructureAnalysis.StructureAnalyzersManager
import matching.regexp.RegExpStyle
import matching.regexp._
import matching.tool.Analysis
import matching.tool.Analysis.{Success => _, Timeout => _, _}
import matching.tool.Debug
import matching.transition.BacktrackMethod.BDM
import matching.transition.BacktrackMethod.KM
import matching.transition._

import scala.io.Source
import scala.util.Failure
import scala.util.Try
import scala.util.control.Exception.nonFatalCatch
import scala.util.{Success => Succ}

/** Settings.
  * @param style
  *   Regex style.
  * @param method
  *   Backtrack method.
  * @param timeout
  *   Timeout seconds.
  * @param file
  *   Input file.
  */
case class Settings(
    mode: RunMode = RunMode.AnalyzeMatching,
    style: RegExpStyle = RegExpStyle.Raw,
    method: Option[BacktrackMethod] = Some(KM),
    timeout: Option[Int] = Some(10),
    file: Option[String] = None,
    help: Boolean = false,
) {
  override def toString(): String =
    s"""--- settings ---------------------------
       |style: $style
       |timeout: ${timeout.fold("disabled")(t => s"$t s")}
       |----------------------------------------
       |""".stripMargin

  def inputSource: Source =
    file.fold(Source.stdin)(Source fromFile _)
}
object Settings {
  def parseArgs(args: Array[String]): Either[OptParseError, Settings] =
    parseArgsRec(args.toList, Settings())

  final case class OptParseError(msg: String) extends AnyVal

  private def parseArgsRec(args: List[String], s: Settings): Either[OptParseError, Settings] =
    args match {
      case ("-h" | "--help") :: _ =>
        Right(s.copy(help = true))
      case ("-s" | "--style") :: styleStr :: rest =>
        RegExpStyle
          .withNameLowercaseOnlyOption(styleStr.toLowerCase)
          .fold[Either[OptParseError, Settings]](Left(OptParseError(s"Unknown style: $styleStr")))(
            style => Right(s.copy(style = style))
          )
          .thenParse(rest)
      case ("-t" | "--timeout") :: timeoutStr :: rest =>
        timeoutStr.toIntOption
          .filter(_ > 0)
          .fold[Either[OptParseError, Settings]](
            Left(OptParseError(s"Invalid timeout value: $timeoutStr"))
          )(value => Right(s.copy(timeout = Some(value))))
          .thenParse(rest)
      case ("-m" | "--method") :: methodStr :: rest =>
        BacktrackMethod
          .withNameLowercaseOnlyOption(methodStr.toLowerCase)
          .collect { case m @ (KM | BDM) => m }
          .fold[Either[OptParseError, Settings]](
            Left(OptParseError(s"Unknown method: $methodStr"))
          )(method => Right(s.copy(method = Some(method))))
          .thenParse(rest)
      case ("--structure") :: rest =>
        Right(s.copy(mode = RunMode.AnalyzeStructure)) thenParse rest
      case ("-d" | "--debug") :: rest =>
        Debug.debugModeGlobal = true
        parseArgsRec(rest, s)
      case path :: rest if s.file.isEmpty =>
        Right(s.copy(file = Some(path))) thenParse rest
      case Nil =>
        Right(s)
      case args =>
        Left(OptParseError(s"Invalid argument: ${args mkString " "}"))
    }

  private implicit class SettingsOpt(val self: Either[OptParseError, Settings]) extends AnyVal {
    @inline def thenParse(rest: List[String]): Either[OptParseError, Settings] =
      self.flatMap(parseArgsRec(rest, _))
  }
}

sealed trait RunMode extends EnumEntry
object RunMode {
  case object AnalyzeMatching extends RunMode
  case object AnalyzeStructure extends RunMode
}

object Main {

  private def usage: String =
    """regex-matching-analyzer <options> <input file>
      |  -s,--style [raw|PCRE]
      |  -t,--timeout [number]
      |  -m,--method [KM|BDM]
      |  --structure
      |  -d,--debug
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    Settings.parseArgs(args) match {
      case Left(err) =>
        System.err.println(s"regex-matching-analyzer: failed to parse args: ${err.msg}")
      case Right(settings) if settings.help =>
        println(usage)
      case Right(settings) if settings.mode == RunMode.AnalyzeMatching =>
        analyzeMatchings(settings)
      case Right(settings) =>
        analyzeStructure(settings)
    }
  }

  def analyzeStructure(settings: Settings): Unit = {
    val analyzerManager = StructureAnalyzersManager(
      RegExpStructureAnalysis.HasLookbehind,
      RegExpStructureAnalysis.HasLookbehindWithCapture,
    )
    for (expr <- settings.inputSource.getLines()) {
      parseRegex(expr, settings).fold(
        err => System.err.println(s"Parse error: $expr: ${err.getMessage()}"),
        { case (regex, _) => analyzerManager.analyze(regex) }
      )
    }
    analyzerManager.printResult()
  }

  def analyzeMatchings(settings: Settings): Unit = {
    settings.inputSource
      .getLines()
      .map { parseRegex(_, settings) }
      .map {
        case Succ((r, opt)) => test(r, opt, settings)
        case Failure(e)     => TestResult.Error(e.getMessage())
      }
      .foreach { result => println(s"$result\n") }
  }

  def parseRegex(regexStr: String, settings: Settings): Try[(RegExp[Char], PCREOptions)] =
    nonFatalCatch.withTry {
      settings.style match {
        case RegExpStyle.Raw  => (RegExpParser(regexStr), new PCREOptions())
        case RegExpStyle.PCRE => RegExpParser.parsePCRE(regexStr)
      }
    }

  def test(r: RegExp[Char], options: PCREOptions, settings: Settings): TestResult = {
    runWithLimit(settings.timeout) {
      calcTimeComplexity(r, options, settings.method)
    } match {
      case (Analysis.Success((growthRate, witness, approximated, ruleSize)), time) =>
        Success(growthRate, witness, approximated, ruleSize, time)
      case (Analysis.Failure(message), _) => Skipped(message)
      case (Analysis.Timeout(_), _)       => Timeout
    }
  }

  // def fileInputTest(inputFile: String, settings: Settings): Unit = {
  //   val regExpStrs = IO.loadFile(inputFile).getLines().toSeq
  //   val total = regExpStrs.length

  //   val dateFormat = DateFormat.getDateTimeInstance()
  //   val startTime = dateFormat.format(new Date())

  //   val dirName =
  //     s"""output/${inputFile.replaceAll(raw"\W", "_")}_${startTime.replaceAll(raw"\W", "-")}"""

  //   IO.createDirectory(dirName)

  //   val resultFile = IO.createFile(s"${dirName}/result.txt")
  //   val resultListFile = IO.createFile(s"${dirName}/list.txt")
  //   val summaryFile = IO.createFile(s"${dirName}/summary.txt")
  //   val timeFile = IO.createFile(s"${dirName}/time.txt")

  //   val detailDirNames =
  //     (
  //       List("constant", "linear", "polynomial", "exponential").flatMap(resultStr =>
  //         List(
  //           (resultStr, Some(false)) -> s"${dirName}/${resultStr}",
  //           (resultStr, Some(true)) -> s"${dirName}/approximated/${resultStr}"
  //         )
  //       ) ++
  //         List("timeout", "skipped", "error").flatMap(resultStr =>
  //           List(
  //             (resultStr, None) -> s"${dirName}/${resultStr}"
  //           )
  //         )
  //     ).toMap

  //   IO.createDirectory(s"${dirName}/approximated")
  //   detailDirNames.values.foreach(IO.createDirectory)
  //   val detailFiles = detailDirNames.map { case (key, name) =>
  //     key -> IO.createFile(s"${name}/result.txt")
  //   }.toMap
  //   val detailListFiles = detailDirNames.map { case (key, name) =>
  //     key -> IO.createFile(s"${name}/list.txt")
  //   }.toMap
  //   val degreeFiles = MTMap[(Int, Boolean), File]()
  //   val degreeListFiles = MTMap[(Int, Boolean), File]()

  //   val summaryCount = MTMap[(String, Option[Boolean]), Int]().withDefaultValue(0)
  //   val degreeCount = MTMap[(Int, Boolean), Int]().withDefaultValue(0)

  //   def writeResult(regExpStr: String, result: TestResult): Unit = {
  //     val resultStr = result match {
  //       case Success(d, _, _, _, _) =>
  //         d match {
  //           case Some(0) => "constant"
  //           case Some(1) => "linear"
  //           case Some(_) => "polynomial"
  //           case None    => "exponential"
  //         }
  //       case Skipped(_) => "skipped"
  //       case Error(_)   => "error"
  //       case Timeout    => "timeout"
  //     }

  //     val approximated = result match {
  //       case Success(_, _, b, _, _) => Some(b)
  //       case _                      => None
  //     }

  //     println(result.toString)
  //     println()

  //     resultFile.writeln(regExpStr)
  //     resultFile.writeln(result.toString)
  //     resultFile.writeln()
  //     resultListFile.writeln(regExpStr)

  //     detailFiles((resultStr, approximated)).writeln(regExpStr)
  //     detailFiles((resultStr, approximated)).writeln(result.toString)
  //     detailFiles((resultStr, approximated)).writeln()
  //     detailListFiles((resultStr, approximated)).writeln(regExpStr)

  //     result match {
  //       case s: Success =>
  //         timeFile.writeln(s.getTime())
  //       case _ => // NOP
  //     }

  //     summaryCount((resultStr, approximated)) += 1

  //     result match {
  //       case Success(Some(d), _, b, _, _) if d >= 2 =>
  //         if (!degreeFiles.contains((d, b))) {
  //           IO.createDirectory(s"${detailDirNames(("polynomial", Some(b)))}/degree_${d}")
  //           degreeFiles += (d, b) -> IO.createFile(
  //             s"${detailDirNames(("polynomial", Some(b)))}/degree_${d}/result.txt"
  //           )
  //           degreeListFiles += (d, b) -> IO.createFile(
  //             s"${detailDirNames(("polynomial", Some(b)))}/degree_${d}/list.txt"
  //           )
  //         }

  //         degreeFiles((d, b)).writeln(regExpStr)
  //         degreeFiles((d, b)).writeln(result.toString)
  //         degreeFiles((d, b)).writeln()
  //         degreeListFiles((d, b)).writeln(regExpStr)

  //         degreeCount((d, b)) += 1
  //       case _ => // NOP
  //     }
  //   }

  //   val started = new Date().getTime()

  //   def printProgress(idx: Int): Unit = {
  //     var expected: Date = null
  //     if (idx > 0) {
  //       val now = new Date().getTime()
  //       val average = (now - started) / idx
  //       expected = new Date(now + average * (total - idx))
  //     }
  //     println(
  //       s"${idx + 1}/${total}, estimated to finish at: ${if (expected != null) dateFormat.format(expected)
  //       else "--------"}"
  //     )
  //   }

  //   val analyzerManager = RegexStructureAnalyzerManager(
  //     RegExpStructureAnalysis.HasLookbehind,
  //     RegExpStructureAnalysis.HasLookbehindWithCapture
  //   )

  //   regExpStrs.zipWithIndex.foreach { case (regExpStr, idx) =>
  //     printProgress(idx)
  //     println(regExpStr)
  //     val parsedRegexTry = parseRegex(regExpStr, settings)
  //     parsedRegexTry.foreach { case (r, _) => analyzerManager.analyze(r) }
  //     val result = parsedRegexTry.fold(
  //       err => TestResult.Error(err.getMessage()),
  //       { case (r, opt) => test(r, opt, settings) }
  //     )
  //     writeResult(regExpStr, result)
  //   }

  //   val finishTime = dateFormat.format(new Date())

  //   summaryFile.writeln(s"input file : ${inputFile}")
  //   summaryFile.writeln(s"started at : ${startTime}")
  //   summaryFile.writeln(s"finished at: ${finishTime}")
  //   summaryFile.writeln()

  //   summaryFile.writeln(settings.toString)
  //   summaryFile.writeln()

  //   summaryFile.writeln(s"${"-" * 3} result ${"-" * 29}")
  //   summaryFile.writeln(f"${"total"}%-12s: ${total}")

  //   summaryFile.writeln()
  //   List("constant", "linear", "polynomial").foreach { resultStr =>
  //     summaryFile.writeln(f"${resultStr}%-12s: ${summaryCount((resultStr, Some(false)))}")
  //   }
  //   degreeCount.toSeq
  //     .collect {
  //       case (d, count) if !d._2 => (d._1, count)
  //     }
  //     .sortBy(_._1)
  //     .foreach { case (degree, count) =>
  //       summaryFile.writeln(s"degree ${degree}: ${count}", 10)
  //     }
  //   summaryFile.writeln(s"exponential : ${summaryCount(("exponential", Some(false)))}")

  //   val approximatedCount = summaryCount.view.filterKeys(_._2 == Some(true)).values.sum
  //   if (approximatedCount > 0) {
  //     summaryFile.writeln()
  //     summaryFile.writeln(s"approximated: ${approximatedCount}")
  //     List("constant", "linear", "polynomial").foreach { resultStr =>
  //       summaryFile.writeln(f"${resultStr}%-11s: ${summaryCount((resultStr, Some(true)))}", 1)
  //     }
  //     degreeCount.toSeq
  //       .collect {
  //         case (d, count) if d._2 => (d._1, count)
  //       }
  //       .sortBy(_._1)
  //       .foreach { case (degree, count) =>
  //         summaryFile.writeln(s"degree ${degree}: ${count}", 10)
  //       }
  //     summaryFile.writeln(s"exponential: ${summaryCount(("exponential", Some(true)))}", 1)
  //     summaryFile.writeln()
  //   }

  //   List("timeout", "skipped", "error").foreach { resultStr =>
  //     summaryFile.writeln(f"${resultStr}%-12s: ${summaryCount((resultStr, None))}")
  //   }
  //   summaryFile.writeln(s"${"-" * 40}")

  //   analyzerManager.printResult(summaryFile)

  //   resultFile.close()
  //   resultListFile.close()
  //   timeFile.close()
  //   summaryFile.close()
  //   detailFiles.values.foreach(_.close())
  //   detailListFiles.values.foreach(_.close())
  //   degreeFiles.values.foreach(_.close())
  //   degreeListFiles.values.foreach(_.close())
  // }
}
