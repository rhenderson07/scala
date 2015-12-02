package problem2

/**
 * Sometimes you have to round up to the next number rather than follow standard rounding rules.
 * Calculate gallons of paint needed to paint the ceiling of a room. Prompt for the length and
 * width, and assume one gallon covers 350 square feet. Display the number of gallons needed to
 * paint the ceiling as a whole number.
 *
 * Challenges:
 * 1) Revise the program to ensure that inputs are entered as numeric values. Don’t allow the
 * user to proceed if the value entered is not numeric.
 * 
 * 2) Implement support for a round room.
 * 3) Implement support for an L-shaped room.
 * 4) Implement a mobile version of this app so it can be used at the hardware store.
 */
object PaintCalculator extends App {

  val AreaPerGallon = 350

  // Prompt user for Int input.
  def promptNonNegativeInt(): Int = {
    try {
      val userInput = scala.io.StdIn.readInt()
      if (userInput >= 0) {
        userInput.toInt
      } else {
        throw new NumberFormatException
      }
    } catch {
      case ioe: NumberFormatException =>
        print("You must input a non-negative integer value to continue. ")
        promptNonNegativeInt
    }
  }

  def determineGallonsNeeded(area: Int): Int = {
    (area.doubleValue() / AreaPerGallon).ceil.toInt
  }

  def outputMessage(area: Int): String = {
    val gallons = determineGallonsNeeded(area)
    val pluralEnding = if (gallons > 1) "s" else ""
    s"You will need to purchase $gallons gallon$pluralEnding of paint to cover $area square feet."
  }

  def run = {
    print("How many square feet are you painting? ")
    val area = promptNonNegativeInt()
    println(outputMessage(area))
  }

  run
}