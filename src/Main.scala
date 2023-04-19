package matching

import matching.TestResult._
import matching.regexp.RegExp._
import matching.regexp.RegExpStructureAnalysis.StructureAnalyzer
import matching.regexp._
import matching.tool.Analysis
import matching.tool.Analysis.{Success => _, Timeout => _, _}
import matching.tool.Debug
import matching.tool.File
import matching.tool.IO
import matching.transition._

import java.text.DateFormat
import java.util.Date
import scala.collection.mutable.{Map => MTMap}
import scala.collection.mutable.{Set => MSet}
import scala.io.Source
import scala.util.Failure
import scala.util.Try
import scala.util.control.Exception.nonFatalCatch
import scala.util.{Success => Succ}

object Main {
  class Settings() {
    var style: RegExpStyle = Raw
    var method: Option[BacktrackMethod] = Some(KM)
    var timeout: Option[Int] = Some(10)

    override def toString(): String =
      s"""--- settings ---------------------------
         |style: $style
         |timeout: ${if (timeout.isDefined) s"${timeout.get}s" else "disabled"}
         |----------------------------------------
         |""".stripMargin
  }

  def main(args: Array[String]): Unit = {
    def parseArgs(rawArgs: Array[String]): (Option[String], Settings) = {
      def parseOptions(options: List[String], setting: Settings = new Settings()): Settings = {
        options match {
          case "--style" :: style :: options =>
            setting.style = style match {
              case "raw"  => Raw
              case "PCRE" => PCRE
              case _      => throw new Exception(s"invalid style option: ${style}")
            }
            parseOptions(options, setting)
          case "--timeout" :: timeout :: options =>
            val t =
              try {
                timeout.toInt
              } catch {
                case _: NumberFormatException =>
                  throw new Exception(s"invalid timeout option: ${timeout}")
              }
            setting.timeout = if (t > 0) Some(t) else None
            parseOptions(options, setting)
          case "--debug" :: options =>
            Debug.debugModeGlobal = true
            parseOptions(options, setting)
          case Nil => setting
          case "--method" :: method :: options =>
            setting.method = method match {
              case "BDM" => Some(BDM)
              case "KM"  => Some(KM)
              case _     => throw new Exception(s"invalid method option: ${method}")
            }
            parseOptions(options, setting)
          case _ => throw new Exception("invalid option")
        }
      }

      val (args, optionStrs) = rawArgs.toList.span(!_.startsWith("--"))
      val inputFile = args match {
        case arg :: Nil => Some(arg)
        case Nil        => None
        case _          => throw new Exception("invalid arguments")
      }

      (inputFile, parseOptions(optionStrs))
    }

    val (inputFile, settings) = parseArgs(args)

    println(settings)

    inputFile match {
      case Some(inputFile) => fileInputTest(inputFile, settings)
      case None            => interactiveTest(settings)
    }
  }

  def parseRegex(regexStr: String, settings: Settings): Try[(RegExp[Char], PCREOptions)] =
    nonFatalCatch.withTry {
      settings.style match {
        case Raw  => (RegExpParser(regexStr), new PCREOptions())
        case PCRE => RegExpParser.parsePCRE(regexStr)
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

  def interactiveTest(settings: Settings): Unit = {
    println("please input expression. (input blank line to quit)")
    Source.stdin
      .getLines()
      .takeWhile { _.nonEmpty }
      .map { parseRegex(_, settings) }
      .map {
        case Succ((r, opt)) => test(r, opt, settings)
        case Failure(e)     => TestResult.Error(e.getMessage())
      }
      .foreach { result => println(s"$result\n") }
  }

  def fileInputTest(inputFile: String, settings: Settings): Unit = {
    val regExpStrs = IO.loadFile(inputFile).getLines().toSeq
    val total = regExpStrs.length

    val dateFormat = DateFormat.getDateTimeInstance()
    val startTime = dateFormat.format(new Date())

    val dirName =
      s"""output/${inputFile.replaceAll(raw"\W", "_")}_${startTime.replaceAll(raw"\W", "-")}"""

    IO.createDirectory(dirName)

    val resultFile = IO.createFile(s"${dirName}/result.txt")
    val resultListFile = IO.createFile(s"${dirName}/list.txt")
    val summaryFile = IO.createFile(s"${dirName}/summary.txt")
    val timeFile = IO.createFile(s"${dirName}/time.txt")

    val detailDirNames =
      (
        List("constant", "linear", "polynomial", "exponential").flatMap(resultStr =>
          List(
            (resultStr, Some(false)) -> s"${dirName}/${resultStr}",
            (resultStr, Some(true)) -> s"${dirName}/approximated/${resultStr}"
          )
        ) ++
          List("timeout", "skipped", "error").flatMap(resultStr =>
            List(
              (resultStr, None) -> s"${dirName}/${resultStr}"
            )
          )
      ).toMap

    IO.createDirectory(s"${dirName}/approximated")
    detailDirNames.values.foreach(IO.createDirectory)
    val detailFiles = detailDirNames.map { case (key, name) =>
      key -> IO.createFile(s"${name}/result.txt")
    }.toMap
    val detailListFiles = detailDirNames.map { case (key, name) =>
      key -> IO.createFile(s"${name}/list.txt")
    }.toMap
    val degreeFiles = MTMap[(Int, Boolean), File]()
    val degreeListFiles = MTMap[(Int, Boolean), File]()

    val summaryCount = MTMap[(String, Option[Boolean]), Int]().withDefaultValue(0)
    val degreeCount = MTMap[(Int, Boolean), Int]().withDefaultValue(0)

    def writeResult(regExpStr: String, result: TestResult): Unit = {
      val resultStr = result match {
        case Success(d, _, _, _, _) =>
          d match {
            case Some(0) => "constant"
            case Some(1) => "linear"
            case Some(_) => "polynomial"
            case None    => "exponential"
          }
        case Skipped(_) => "skipped"
        case Error(_)   => "error"
        case Timeout    => "timeout"
      }

      val approximated = result match {
        case Success(_, _, b, _, _) => Some(b)
        case _                      => None
      }

      println(result.toString)
      println()

      resultFile.writeln(regExpStr)
      resultFile.writeln(result.toString)
      resultFile.writeln()
      resultListFile.writeln(regExpStr)

      detailFiles((resultStr, approximated)).writeln(regExpStr)
      detailFiles((resultStr, approximated)).writeln(result.toString)
      detailFiles((resultStr, approximated)).writeln()
      detailListFiles((resultStr, approximated)).writeln(regExpStr)

      result match {
        case s: Success =>
          timeFile.writeln(s.getTime())
        case _ => // NOP
      }

      summaryCount((resultStr, approximated)) += 1

      result match {
        case Success(Some(d), _, b, _, _) if d >= 2 =>
          if (!degreeFiles.contains((d, b))) {
            IO.createDirectory(s"${detailDirNames(("polynomial", Some(b)))}/degree_${d}")
            degreeFiles += (d, b) -> IO.createFile(
              s"${detailDirNames(("polynomial", Some(b)))}/degree_${d}/result.txt"
            )
            degreeListFiles += (d, b) -> IO.createFile(
              s"${detailDirNames(("polynomial", Some(b)))}/degree_${d}/list.txt"
            )
          }

          degreeFiles((d, b)).writeln(regExpStr)
          degreeFiles((d, b)).writeln(result.toString)
          degreeFiles((d, b)).writeln()
          degreeListFiles((d, b)).writeln(regExpStr)

          degreeCount((d, b)) += 1
        case _ => // NOP
      }
    }

    val started = new Date().getTime()

    def printProgress(idx: Int): Unit = {
      var expected: Date = null
      if (idx > 0) {
        val now = new Date().getTime()
        val average = (now - started) / idx
        expected = new Date(now + average * (total - idx))
      }
      println(
        s"${idx + 1}/${total}, estimated to finish at: ${if (expected != null) dateFormat.format(expected)
        else "--------"}"
      )
    }

    val analyzerManager = RegexStructureAnalyzerManager(
      RegExpStructureAnalysis.HasLookbehind,
      RegExpStructureAnalysis.HasLookbehindWithCapture
    )

    regExpStrs.zipWithIndex.foreach { case (regExpStr, idx) =>
      printProgress(idx)
      println(regExpStr)
      val parsedRegexTry = parseRegex(regExpStr, settings)
      parsedRegexTry.foreach { case (r, _) => analyzerManager.analyze(r) }
      val result = parsedRegexTry.fold(
        err => TestResult.Error(err.getMessage()),
        { case (r, opt) => test(r, opt, settings) }
      )
      writeResult(regExpStr, result)
    }

    val finishTime = dateFormat.format(new Date())

    summaryFile.writeln(s"input file : ${inputFile}")
    summaryFile.writeln(s"started at : ${startTime}")
    summaryFile.writeln(s"finished at: ${finishTime}")
    summaryFile.writeln()

    summaryFile.writeln(settings.toString)
    summaryFile.writeln()

    summaryFile.writeln(s"${"-" * 3} result ${"-" * 29}")
    summaryFile.writeln(f"${"total"}%-12s: ${total}")

    summaryFile.writeln()
    List("constant", "linear", "polynomial").foreach { resultStr =>
      summaryFile.writeln(f"${resultStr}%-12s: ${summaryCount((resultStr, Some(false)))}")
    }
    degreeCount.toSeq
      .collect {
        case (d, count) if !d._2 => (d._1, count)
      }
      .sortBy(_._1)
      .foreach { case (degree, count) =>
        summaryFile.writeln(s"degree ${degree}: ${count}", 10)
      }
    summaryFile.writeln(s"exponential : ${summaryCount(("exponential", Some(false)))}")

    val approximatedCount = summaryCount.view.filterKeys(_._2 == Some(true)).values.sum
    if (approximatedCount > 0) {
      summaryFile.writeln()
      summaryFile.writeln(s"approximated: ${approximatedCount}")
      List("constant", "linear", "polynomial").foreach { resultStr =>
        summaryFile.writeln(f"${resultStr}%-11s: ${summaryCount((resultStr, Some(true)))}", 1)
      }
      degreeCount.toSeq
        .collect {
          case (d, count) if d._2 => (d._1, count)
        }
        .sortBy(_._1)
        .foreach { case (degree, count) =>
          summaryFile.writeln(s"degree ${degree}: ${count}", 10)
        }
      summaryFile.writeln(s"exponential: ${summaryCount(("exponential", Some(true)))}", 1)
      summaryFile.writeln()
    }

    List("timeout", "skipped", "error").foreach { resultStr =>
      summaryFile.writeln(f"${resultStr}%-12s: ${summaryCount((resultStr, None))}")
    }
    summaryFile.writeln(s"${"-" * 40}")

    analyzerManager.printResult(summaryFile)

    resultFile.close()
    resultListFile.close()
    timeFile.close()
    summaryFile.close()
    detailFiles.values.foreach(_.close())
    detailListFiles.values.foreach(_.close())
    degreeFiles.values.foreach(_.close())
    degreeListFiles.values.foreach(_.close())
  }

  class RegexStructureAnalyzersManager(
      analyzers: Seq[StructureAnalyzer[Char, RegExp[Char]]]
  ) {
    private val agg: Seq[MSet[RegExp[Char]]] = Vector.fill(analyzers.size)(MSet())
    private val zipped = analyzers zip agg
    def analyze(r: RegExp[Char]): Unit =
      zipped.foreach { case (analyzer, set) =>
        if (analyzer.analyze(r)) {
          set += r
        }
      }
    def printResult(file: File): Unit = {
      file.writeln("--- Structure Analysis --------------------------")
      zipped.foreach { case (analyzer, set) =>
        file.writeln(s"==> ${analyzer.name}")
        file.writeln(s"  -> Total: ${set.size}")
        set.foreach { r => file.writeln(s"      ${r.toString}") }
        file.writeln()
      }
      file.writeln("-------------------------------------------------")
    }
  }
  object RegexStructureAnalyzerManager {
    def apply(analyzers: StructureAnalyzer[Char, RegExp[Char]]*): RegexStructureAnalyzersManager =
      new RegexStructureAnalyzersManager(analyzers)
  }
}
