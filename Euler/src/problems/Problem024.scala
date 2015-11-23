
package problems

import scala.annotation.tailrec
import common.MyMath

object Problem024 extends Problem with App {
  def number = 24
  def description = "What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?"
  def run = permAt(target, position).toLong

  val target = "0123456789"
  val position = 1000000
  
  def perms(str : String) = str.permutations
  def permAt(s: String, index : Int) = perms(s).take(index).max
  
  println(run)
}