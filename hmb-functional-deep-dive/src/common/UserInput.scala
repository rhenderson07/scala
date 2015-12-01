package common

object UserInput {

  // Prompt user for input.
  def promptString(): String = {
    val userInput = scala.io.StdIn.readLine()

    if (userInput.nonEmpty) {
      userInput
    } else {
      print("You must input a string to continue. ")
      promptString
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
}