

class Problem4 extends Problem {
  def number = 4
  def description = "Find the largest palindrome made from the product of two 3-digit numbers."

  def run = largestPalindrome

  // my attempt, with lists and recursion
  def products(nums: List[Int]): List[Int] = {
    def rec(l: List[Int]): List[Int] = {
      if (l.length < 2) {
        List.empty
      } else {
        l.map(_ * l.head) ++ rec(l.tail)
      }
    }
    rec(nums).distinct
  }

  def isPalindrome(str: String): Boolean = str.equals(str.reverse)
  val largestPalindrome = products((100 to 999).toList).filter((x: Int) => isPalindrome(x.toString)).max

  // from euler, with for-expression
  val largestPalindrome2 = (for (x <- 100 to 999; y <- 100 to 999; z = x * y; if z.toString.equals(z.toString.reverse)) yield z).max
}

object Prob4Main extends App {
  val p = new Problem4
  //p.products((100 to 999).toList).foreach(println)

  println(p.largestPalindrome2)
}