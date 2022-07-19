package matching
import regexp._
import tool.{IO,File}

object MakeExample{
  //正規表現のリストから新たなリストを作成
  def containBR[A](r:RegExp[A]): Boolean = {
    //バックリファレンスが含まれるか否か
    r match{
      case BackReferenceExp(n, name) => true
      case ConcatExp(r1,r2) => containBR(r1) || containBR(r2)
      case GroupExp(r, id, name) => containBR(r)
      case AltExp(r1,r2) => containBR(r1) || containBR(r2)
      case StarExp(r,greedy) => containBR(r)
      case PlusExp(r,greedy) => containBR(r)
      case OptionExp(r,greedy) => containBR(r)
      case RepeatExp(r,min,max,greedy) => containBR(r)
      case IfExp(cond,rt,rf) => containBR(cond) || containBR(rt) || containBR(rf)
      case LookaheadExp(r, positive) => containBR(r)
      case LookbehindExp(r, positive) => containBR(r)
      case _ => false
    }

  }
  def make(args: Array[String]){
    val filepath1 = s"/Users/kawamura/Research/regex-matching-analyzer/input/nakagawa/nakagawa_processed.txt"
    val filepath2 = s"/Users/kawamura/Research/regex-matching-analyzer/input/regexlib/regexlib_processed.txt"
    val filepath3 = s"/Users/kawamura/Research/regex-matching-analyzer/input/snort/snort_processed.txt"

    val regExpStrs1 = IO.loadFile(filepath1).getLines.toSeq
    val regExpStrs2 = IO.loadFile(filepath2).getLines.toSeq
    val regExpStrs3 = IO.loadFile(filepath3).getLines.toSeq
    val output1 = new File(s"/Users/kawamura/Research/regex-matching-analyzer/input/BR_nakagawa.txt")
    for(regstr <- regExpStrs1){
      try{
        val reg = RegExpParser.parsePCRE(regstr)
        if(containBR(reg._1)){
          output1.writeln(regstr)
        }
      }
      catch{
        case e: Exception =>
      }
    }
    /*
    val output2 = new File(s"/Users/kawamura/Research/regex-matching-analyzer/input/BR_regexlib.txt")
    for(regstr2 <- regExpStrs2){
      try{
        val reg = RegExpParser.parsePCRE(regstr2)
        if(containBR(reg._1)){
          output2.writeln(regstr2)
        }
      }
      catch{
        case e: Exception =>
      }
    }

    val output3 = new File(s"/Users/kawamura/Research/regex-matching-analyzer/input/BR_snort_short.txt")
    var count = 0
    for(regstr3 <- regExpStrs3){
      try{
        val reg = RegExpParser.parsePCRE(regstr3)
        if(containBR(reg._1) && count <= 500){
          count += 1
          output3.writeln(regstr3)
        }
      }
      catch{
        case e: Exception =>
      }
    }
    */


  }
}
