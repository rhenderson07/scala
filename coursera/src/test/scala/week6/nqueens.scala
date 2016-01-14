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
          if isSafe(row, queens)
        } yield row :: queens
      }
    }
    placeQueens(n)
  }

  def isSafe(row: Int, queens: List[Int]): Boolean = {
    // not possible for same column to exist in list
    // test if same row exists in list
    // test if new queen is threatened diagonally

    val gameState = queens.zip(Stream.from(1))
    gameState.forall {
      case (r, c) => row != r && math.abs(row - r) != c
    }
  }

  def show(queens: List[Int]) = {
    // sort on rows
    val pairs = queens.zipWithIndex.sorted

    val rows = {
      for (p <- pairs)
        yield Vector.fill(pairs.length)("* ").updated(p._2, "X ").mkString

    }
    "\n" + rows.mkString("\n") + "\n"
  }

  val boardSize = 12
  queens(boardSize).par.take(3).map(s => s.mkString(",") + "\n" + show(s)).foreach(println)
}