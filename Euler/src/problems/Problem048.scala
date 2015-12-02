
package problems

object Problem048 extends Problem with App {
  def number = 48
  def description = "Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000"

  lazy val run = Stream.from(1).take(1000).map(selfPower(lastTen)(_)).reduceLeft((a, b) => lastTen(a + b)).longValue()

  def selfPower(truncateFunction: (BigInt => BigInt))(n: Int): BigInt = {
    def rec(multiplier: BigInt, timesToMultiply: Int, runningTotal: BigInt = 1): BigInt = {
      if (timesToMultiply == 0 || runningTotal == 0) {
        runningTotal
      } else {
        val newTotal = truncateFunction(runningTotal * multiplier)
        rec(multiplier, timesToMultiply - 1, newTotal)
      }
    }

    rec(n, n)
  }

  val lastTen = lastDigits(10)_

  def lastDigits(maxLength: Int)(n: BigInt): BigInt = {
    val intStr = n.toString()

    if (intStr.length() > maxLength)
      BigInt(intStr.substring(intStr.length - maxLength))
    else
      n
  }
  
  // solution from euler. Much faster
  val sol2 = (1 to 1000).map(x => BigInt(x).pow(x)).sum % BigInt(10).pow(10)

  println(run)
  println(sol2)
}