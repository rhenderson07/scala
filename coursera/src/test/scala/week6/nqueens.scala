package week6

import scala.annotation.tailrec

object nqueens extends App {

  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else {
        for {
          queens <- placeQueens(k - 1)
          row <- 0 until n
          val size = n - 1
          if isSafe(row, queens)
          if (row <= size / 2 || !isSafe(size - row, queens))
        } yield row :: queens
      }
    }
    //find solutions and sort so low numbers appear in list first
    placeQueens(n).map(_.reverse)
  }

  /**
   * Return false if same row exists in list or if new queen is threatened diagonally.
   * Not possible for same column to exist in list.
   */
  def isSafe(row: Int, queens: List[Int]): Boolean = {
    val gameState = queens.zip(Stream.from(1))
    gameState.forall {
      case (r, c) => row != r && math.abs(row - r) != c
    }
  }

  def show(queens: List[Int]) = {
    // sort on rows
    val pairs = queens.zipWithIndex.sorted

    val rows = {
      for (p <- queens.zipWithIndex.sorted)
        yield Vector.fill(pairs.length)("* ").updated(p._2, "X ").mkString

    }
    "\n" + rows.mkString("\n") + "\n"
  }

  def time[A <: Any, B <: Any](f: (A => B))(param: A): Unit = {
    val startTime = System.nanoTime();
    val solution = f(param)
    val endTime = System.nanoTime();
    println(s"Solution = $solution.  Elapsed time ${(endTime - startTime) * 1e-9} seconds\n")
  }

  // testing
  val boardSize = 10
  def queenSolutions(n: Int): Int = queens(n).size

  time(queenSolutions)(boardSize)
  queens(boardSize).take(3).map(s => s.mkString(",") + "\n" + show(s)).foreach(println)
}