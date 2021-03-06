
package problems

import scala.annotation.tailrec
import common.math.MyMath

object Problem024 extends Problem with App {
  def number = 24
  def description = "What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?"
  lazy val run = permAt(target, position).toLong

  lazy val target = "0123456789"
  lazy val position = 1000000
  
  def perms(str : String) = str.permutations
  def permAt(s: String, index : Int) = perms(s).take(index).max
  
  println(run)
}