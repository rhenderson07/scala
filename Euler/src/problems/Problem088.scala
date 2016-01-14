package problems

import common.math.Primes
import common.upgrades.Implicits._
import common.math.MyMath
import scala.annotation.tailrec

object Problem088 extends Problem with App {
  def number = 88
  def description = "What is the sum of all the minimal product-sum numbers for 2 =< k =< 12000?"

  def run = -1L

  /**
   * For given value k, returns minimal sum product composed of that many elements
   */
  def minimalSumProduct(k: Int): Int = {
    Stream.from(1).map(i => sumProduct(k)(i)).find(_.nonEmpty).get.get.sum
  }

  def isSumProduct(l: List[Int]) = {
    l.sum == l.product
  }

  /**
   * For the given value x, find all sum product sets
   */
  def sumProduct(length: Int)(x: Int): Option[List[Int]] = {
    // get divisors in reverse order
    val divisors = MyMath.findDivisors(x).sorted.map(_.toInt)

    comb(length, divisors).find(isSumProduct)
  }

  private def combInner[T](n: Int, l: List[T]): List[List[T]] =
    n match {
      case 0 => List(List())
      case _ => for (
        element <- l;
        sl <- combInner(n - 1, l.dropWhile(_ != element))
      ) yield element :: sl
    }

  def comb[T](n: Int, l: List[T]): List[List[T]] = combInner(n, l.distinct)

  //  MyMath.findDivisors(156).sorted.reverse.combinations(3).foreach(println)
  //  sumProduct(2)(2).foreach(println)
  println(minimalSumProduct(6))
}