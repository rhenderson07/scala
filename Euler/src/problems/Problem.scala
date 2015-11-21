

package problems

trait Problem {
  def number : Int
  def name : String = "Problem " + number
  def description : String
  def run : Long
}