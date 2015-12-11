package advanced.chess

class Queens {

  /**
   * Find all solutions to the n-Queens problem for a given value n. Return a list of lists.
   * Each list in the return list is a solution, containing n grid points.
   */
  def queens(n: Int): List[List[(Int, Int)]] = {

    //Generate all partial solutions of length k in a list. Every element of the list is one
    //solution, represented by a list of length k. So placeQueens returns a list of lists.
    def placeQueens(k: Int): List[List[(Int, Int)]] = {
      if (k == 0)
        List(List())
      else
        for {
          queens <- placeQueens(k - 1)
          column <- 1 to n
          queen = (k, column)
          if isSafe(queen, queens)
        } yield queen :: queens
    }

    placeQueens(n).map(_.reverse)
  }

  def isSafe(queen: (Int, Int), queens: List[(Int, Int)]) =
    queens forall (q => !inCheck(queen, q))

  def inCheck(q1: (Int, Int), q2: (Int, Int)) = {
    q1._1 == q2._1 || // same row
      q1._2 == q2._2 || // same column
      (q1._1 - q2._1).abs == (q1._2 - q2._2).abs // on diagonal
  }

}