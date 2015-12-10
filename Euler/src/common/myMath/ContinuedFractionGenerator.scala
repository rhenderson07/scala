package common.myMath

import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.int2bigDecimal
import scala.collection.LinearSeq
import scala.annotation.tailrec

object ContinuedFractionGenerator {
  private def generateFor(n: BigDecimal): Stream[BigDecimal] = {
    Stream.iterate(n)(nextContinuedFractionPart)
  }

  private def nextContinuedFractionPart(n: BigDecimal): BigDecimal = {
    val context = n.mc
    val wholePart = getWholePart(n)
    val remainder = n - wholePart

    if (remainder.equals(0))
      0
    else
      BigDecimal(1).apply(context) / remainder
  }

  private def getWholePart(n: BigDecimal): Int = {
    n.quot(1).toInt
  }

  def asContinuedFraction(n: BigDecimal): ContinuedFraction = {
    val generator = generateFor(n)
    val nonRepeating = getWholePart(generator.head)
    val repeating = findRepeatingSection(generator.tail)

    new ContinuedFraction(nonRepeating, repeating)
  }

  private def findRepeatingSection(s: LinearSeq[BigDecimal]): List[Int] = {
    @tailrec
    def rec(seq: LinearSeq[BigDecimal], repeatingSoFar: LinearSeq[BigDecimal]): LinearSeq[BigDecimal] = {
      val candTruncate = seq.head.setScale(10, RoundingMode.HALF_UP)

      if (repeatingSoFar.contains(candTruncate)) {
        repeatingSoFar
      } else {
        rec(seq.tail, repeatingSoFar :+ candTruncate)
      }
    }

    val repeatRaw = rec(s, List())

    if (repeatRaw.equals(List(0)))
      List() // if the repeating value is zero, return an empty list
    else
      repeatRaw.map(getWholePart).toList
  }
}