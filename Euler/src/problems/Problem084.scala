
package problems

import common.math.MyMath

object Problem084 extends Problem with App {
  def number = 84
  def description = "Monopoly odds: Problem defined at https://projecteuler.net/problem=84"

  lazy val run = -1L

  val monopolyCircuit = Seq("GO", "A1", "CC1", "A2", "T1", "R1", "B1", "CH1", "B2", "B3", "JAIL",
    "C1", "U1", "C2", "C3", "R2", "D1", "CC2", "D2", "D3", "FP", "E1", "CH2", "E2", "E3", "R3",
    "F1", "F2", "U2", "F3", "G2J", "G1", "G2", "CC3", "G3", "R4", "CH3", "H1", "T2", "H2");

  val communityChestCards = {
    val TotalCount = 16
    val special = List("GO", "JAIL")
    List.fill(TotalCount - special.size)("") :: special
  }

  val chanceCards = {
    val TotalCount = 16
    val special = List("GO", "JAIL", "")
    List.fill(TotalCount - special.size)("") :: special
  }

  //Planning:
  // I think the best way to go about this is to look at each position and calculate for each other pos,
  // what are the odds of ending up at the target pos, from there.

  //Dice.diceFeq(List(6, 6, 6, 6)).foreach(println)
  communityChestCards.take(50).foreach(println)
}

object Dice {
  /**
   * List of possible outcomes for a given set of die
   */
  private def diceOutcomes(dice: List[Int]) = {
    def rec(remainingDice: List[Int], outcomes: List[Int]): List[Int] = {
      if (remainingDice.isEmpty) {
        outcomes
      } else {
        lazy val range = (1 to remainingDice.head)
        lazy val newOutcomes = outcomes.map(x => range.map(d => x + d)).flatten
        rec(remainingDice.tail, newOutcomes)
      }
    }
    rec(dice, List(0)).sorted
  }

  /**
   * Return a list of frequencies for each element in the list
   */
  private def frequency[T <% Ordered[T]](l: List[T]) = {
    val totalCount = l.size
    l.groupBy(identity).mapValues(_.size * BigDecimal(100) / totalCount).toList.sortBy(_._1)
  }

  def diceFeq(dice: List[Int]) = frequency(diceOutcomes(dice))
}