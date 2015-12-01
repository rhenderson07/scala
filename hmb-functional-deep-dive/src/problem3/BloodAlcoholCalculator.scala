package problem3

import scala.math.BigDecimal

/**
 * Sometimes you have to perform a more complex calculation based on some provided inputs and then
 * use that result to make a determination.
 *
 * Create a program that prompts for your weight, gender, and number of drinks, the amount of
 * alcohol by volume of the drinks consumed, and the amount of time since your last drink.
 * Calculate your blood alcohol content (BAC) using this formula.
 */
object BloodAlcoholCalculator extends App {

  val DistribRatioMale = 0.73
  val DistribRatioFemale = 0.66

  // Prompt user for Double input.
  def promptNonNegativeDouble(): Int = {
    try {
      val userInput = scala.io.StdIn.readDouble()
      if (userInput >= 0) {
        userInput.toInt
      } else {
        throw new NumberFormatException
      }
    } catch {
      case ioe: NumberFormatException =>
        print("You must input a non-negative decimal value to continue. ")
        promptNonNegativeDouble
    }
  }

  def promptYesOrNo(): Boolean = {
    val userInput = scala.io.StdIn.readLine().toLowerCase

    if (List("y", "yes").contains(userInput))
      true
    else if (List("n", "no").contains(userInput))
      false
    else {
      print("You must input either 'y' or 'n' to continue. ")
      promptYesOrNo
    }
  }

  def determineDistribRatio(isMale: Boolean) = {
    if (isMale)
      DistribRatioMale
    else
      DistribRatioFemale
  }

  def bloodAlcoholContent(alcoholConsumed: Double, weight: Double, distribRatio: Double, hoursSinceLastDrink: Double) = {
    (alcoholConsumed * 5.14 / weight * distribRatio) - (.015 * hoursSinceLastDrink)
  }

  def roundDouble(n: Double, decimalPlaces: Int) = {
    BigDecimal(n).setScale(decimalPlaces, BigDecimal.RoundingMode.HALF_UP)
  }

  def outputMessage(BAC: Double): String = {
    val roundedBAC = roundDouble(BAC, 2)
    val legalModifier = if (BAC >= 0.08) " not" else ""

    s"Your BAC is ${roundedBAC}. It is$legalModifier legal for you to drive."
  }

  def run = {
    print("How many ounces of alcohol did you consume?")
    val alcoholConsumed = promptNonNegativeDouble

    print("How much do you weigh in pounds?")
    val weight = promptNonNegativeDouble

    print("Are you biologically male?")
    val isMale: Boolean = promptYesOrNo
    val distribRatio = determineDistribRatio(isMale)

    print("How many hours have passed since your last drink?")
    val hoursSinceLastDrink = promptNonNegativeDouble

    val BAC = bloodAlcoholContent(alcoholConsumed, weight, distribRatio, hoursSinceLastDrink)
    println(outputMessage(BAC))
  }

  run
}