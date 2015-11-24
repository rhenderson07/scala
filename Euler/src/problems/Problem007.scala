package problems
import common.MyMath

object Problem007 extends Problem with App {
  def number = 7
  def description = "What is the 10 001st prime number?"

  def run = MyMath.primeAt(10001)

  println(run)
}