
package problems

import scala.annotation.tailrec

object Problem016 extends Problem with App {
  def number = 16
  def description = "What is the sum of the digits of the number 2^1000?"
  def run = sumOfDigits(largeVal)
    
  lazy val largeVal = BigInt(2).pow(1000)
  
  def sumOfDigits(n :BigInt) = {
    n.toString.toList.map(_.asDigit).reduce(_ + _)
  }
    
  println(run)
}