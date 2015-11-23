
package problems

import scala.annotation.tailrec

object Problem020 extends Problem with App {
  def number = 20
  def description = "Find the sum of the digits in 100 factorial"
  def run = sumOfDigits(largeVal)
    
  lazy val largeVal = BigInt(2).pow(1000)
  
  
  
  
  def sumOfDigits(n :BigInt) = {
    n.toString.toList.map(_.asDigit).reduce(_ + _)
  }
    
  println(run)
}