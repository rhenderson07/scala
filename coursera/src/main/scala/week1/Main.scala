package week1
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 15) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    return balanceHelper(chars, 0) == 0
  }

  def balanceHelper(chars: List[Char], openCount: Int): Int = {
    if (chars.isEmpty)
      0
    else if (openCount < 0)
      -1
    else if (chars.head == '(')
      balanceHelper(chars.tail, openCount + 1)
    else if (chars.head == ')')
      balanceHelper(chars.tail, openCount - 1)
    else
      balanceHelper(chars.tail, openCount)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (coins.isEmpty || money < 0)
      0
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
