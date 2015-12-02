package problem1

/**
 * Create a program that prompts for an input string and displays output that shows the input
 * string and the number of characters the string contains.
 *
 *  Challenges:
 * 1) If the user enters nothing, state that the user must enter something into the program.
 *
 * 2) Implement this program using a graphical user interface and update the character counter
 * every time a key is pressed. If your language doesn’t have a particularly friendly GUI
 * library, try doing this exercise with HTML and JavaScript instead.
 *
 * Extra Credit:
 * 1) Count the number of characters from: http://www.gutenberg.org/cache/epub/11/pg11.txt
 */
object CharacterCounter extends App {
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

  // Return a string that describes the length of the characters
  def outputMessage(str: String): String = {
    val len = str.length()
    val pluralEnding = if (len > 1) "s" else ""

    s"'$str' has $len character$pluralEnding."
  }

  def run = {
    print("What is the input string? ")
    val input = promptString
    println(outputMessage(input))
  }

  // Code in body of App object is run when program is executed
  run
}