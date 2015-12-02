package problem3

import scala.math.BigDecimal
import common.UserInput

/**
 * Sometimes you have to perform a more complex calculation based on some provided inputs and then
 * use that result to make a determination.
 *
 * Create a program that prompts for your weight, gender, and number of drinks, the amount of
 * alcohol by volume of the drinks consumed, and the amount of time since your last drink.
 * Calculate your blood alcohol content (BAC) using this formula.
 *
 * Challenges:
 * 1) Handle metric units.
 *
 * 2)	Look up the legal BAC limit by state and prompt for the state. Display a message that
 * states whether or not it’s legal to drive based on the computed BAC.
 *
 * 3) Develop this as a mobile application that makes it easy to record each drink, updating the
 * BAC each time a drink is entered.
 */
object BloodAlcoholCalculator extends App {

  def determineDistribRatio(isMale: Boolean) = {
    val DistribRatioMale = 0.73
    val DistribRatioFemale = 0.66

    if (isMale)
      DistribRatioMale
    else
      DistribRatioFemale
  }

  def bloodAlcoholContent(alcoholConsumed: Double, weight: Double, distribRatio: Double, hoursSinceLastDrink: Double) = {
    (alcoholConsumed * 5.14 / weight * distribRatio) - (.015 * hoursSinceLastDrink)
  }

  def roundDouble(n: Double, decimalPlaces: Int): BigDecimal = {
    BigDecimal(n).setScale(decimalPlaces, BigDecimal.RoundingMode.HALF_UP)
  }

  def outputMessage(BAC: Double): String = {
    val roundedBAC = roundDouble(BAC, 2)
    val legalModifier = if (BAC >= 0.08) " not" else ""

    s"Your BAC is ${roundedBAC}. It is$legalModifier legal for you to drive."
  }
  
  def run = {
    print("How many ounces of alcohol did you consume? ")
    val alcoholConsumed = UserInput.promptNonNegativeDouble

    print("How much do you weigh in pounds? ")
    val weight = UserInput.promptNonNegativeDouble

    print("Are you biologically male? ")
    val isMale: Boolean = UserInput.promptYesOrNo
    val distribRatio = determineDistribRatio(isMale)

    print("How many hours have passed since your last drink? ")
    val hoursSinceLastDrink = UserInput.promptNonNegativeDouble

    val BAC = bloodAlcoholContent(alcoholConsumed, weight, distribRatio, hoursSinceLastDrink)
    println(outputMessage(BAC))
  }

  run
}