
package problems

import common.math.MyMath
import common.upgrades.Implicits._

object Problem084 extends Problem with App {
  def number = 84
  def description = "Monopoly odds: Problem defined at https://projecteuler.net/problem=84"

  lazy val run = -1L

  val monopolyCircuit = List("GO", "A1", "CC1", "A2", "T1", "R1", "B1", "CH1", "B2", "B3", "JAIL",
    "C1", "U1", "C2", "C3", "R2", "D1", "CC2", "D2", "D3", "FP", "E1", "CH2", "E2", "E3", "R3",
    "F1", "F2", "U2", "F3", "G2J", "G1", "G2", "CC3", "G3", "R4", "CH3", "H1", "T2", "H2");

  //  class MonopolySquare(val name:String, val)

  val communityChestCards = {
    val TotalCount = 16
    val special = List("GO", "JAIL")
    val filler = List.fill[String](TotalCount - special.size)("")
    filler ::: special
  }

  val chanceCards = {

  }

  // enum for ca
  //  abstract class Square(val name:String)
  //  case class Building(val name:String) extends Square;
  //  case class RailRoad(val name:String) extends Square;
  //  case class Chance(val name:String) extends Square;

  // enum for ca
  //  abstract class SquareType extends Enumeration {
  //    type SquareType = Value
  //    val BUILDING, CHANCE, COMMUNITY, RAILROAD, UTILITY, GO, JAIL, GOTOJAIL = Value
  //  }
  //
  //  val chanceCards = {
  //    val TotalCount = 16
  //    val special = List("GO", "JAIL", "")
  //    List.fill(TotalCount - special.size)("") :: special
  //  }

  //Sudo:
  /*
For each square:
	get all single roll outcomes
	for each roll outcome:
		if new location is on a special square:
			modify outcome result
		if roll resulted from doubles:
			increment doubleCount
			if doubleCount >= DOUBLE_LIMIT:
				go to Jail
			else:
				repeat process from new square.
   */

  //Planning:
  // I think the best way to go about this is to look at each position and calculate for each other pos,
  // what are the odds of ending up at the target pos, from there.

  /**
   * Given a square and assuming all squares are normal, what are the possible outcomes and their corresponding probability?
   */
  def outcomesForSquare(rolls: List[Int], circuit: List[String], index: Int): List[String] = {
    rolls.map(x => circuit((x + index) % circuit.size))
  }

  def specialSquareModifier(circuit: List[String], index: Int): List[String] = {
    List()
  }

  // Dice.diceFeq(List(6, 6, 6, 6)).foreach(println)
  // communityChestCards.take(50).foreach(println)
  lazy val test = {
    val rolls = Dice.diceOutcomes(List(6, 6))
    val outcomeFunct = outcomesForSquare(rolls, monopolyCircuit, _: Int)
    val outcomes = (0 until monopolyCircuit.size).map(outcomeFunct).flatten.toList

    outcomes.frequencies.sorted
  }

  test.foreach(println)
}

object Dice {
  /**
   * List of possible outcomes for a given set of die
   */
  def diceOutcomes(dice: List[Int]) = {
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

  def diceFeq(dice: List[Int]) = diceOutcomes(dice).frequencies.sorted
}