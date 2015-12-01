
package problems

object Problem044 extends Problem with App {
  def number = 44
  def description = "Find the pair of pentagonal numbers, Pj and Pk, for which their sum and difference are pentagonal and D = |Pk − Pj| is minimised; what is the value of D?"

  lazy val run = solutionPairDiff

  lazy val pentagonal = Stream.from(1).map(_.longValue).map(n => n * (3 * n - 1) / 2)

  // take 2 numbers and return true if sum and difference are pentagonal
  def sumAndDiffPentagonal(i: Long, j: Long): Boolean = {
    val pentsUnderSum = pentagonal.takeWhile(_ <= i + j)
    pentsUnderSum.par.exists(_ == i + j) && pentsUnderSum.par.exists(_ == Math.abs(i - j))
  }

  def findPentagonalPairs(n: Long) = {
    pentagonal.takeWhile(_ < n).filter(sumAndDiffPentagonal(_, n)).map((_, n))
  }

  lazy val solutionPair = pentagonal.map(findPentagonalPairs(_)).find(_.nonEmpty).get.head

  //println(run)
  //  println(pentagonal(1))

  //check sum and diff works
  //println(sumAndDiffPentagonal(22, 70))

  //check find pentagonal pairs works. 
  // Currently does not return
  //findPentagonalPairs(70).foreach(println)

  //
  lazy val solutionPairDiff = Math.abs(solutionPair._1 - solutionPair._2)
  println(s"Pair: $solutionPair, Diff: $solutionPairDiff")
}