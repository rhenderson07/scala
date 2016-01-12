package problems

import common.math.Primes
import common.upgrades.Implicits._
import scala.annotation.tailrec

object Problem088 extends Problem with App {
  def number = 88
  def description = "What is the sum of all the minimal product-sum numbers for 2 =< k =< 12000?"

  def run = -1L

  /**
   * For given value k, returns minimal sum product composed of that many elements
   */
  def minimalSumProduct(k: Int): Int = {
    -1
  }

  def isSumProduct(l: List[Int]) = {
    l.sum == l.product
  }

  /**
   * For the given value x, find all sum product sets
   */
  def sumProductSets(x: Int): List[List[Int]] = {
    val divisors = common.math.MyMath.findDivisors(x).map(_.toInt)

//    @tailrec
    def rec(factors: List[Int], currentList: List[Int], remaining: Int): List[List[Int]] = {
      if (remaining == 0) {
        List()
      } else {
        List()
      }
    }

    rec(divisors,List(), x)
  }

}