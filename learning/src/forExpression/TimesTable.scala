package forExpression

object TimesTable extends App {

  def timesTable(dimensionSize: Int) = {
    Array.tabulate(dimensionSize, dimensionSize)((row, col) => (row + 1) * (col + 1))
  }

  println(timesTable(10).deep.mkString("\n"))
}