package patternMatching

object PartialFunction extends App {
  def pAnswerUnits: PartialFunction[Any, Any] = {
    case d: Int if d != 0 => 42 / d
    case _ => "NaN"    
  }

  val values = List(1, 2, -1, 42, 0, 6, "apple")
  values.foreach(p => println(p + " -> " + pAnswerUnits(p)))
}