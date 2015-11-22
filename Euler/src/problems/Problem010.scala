package problems
import common.Primes

object Problem10 extends Problem with App {
  def number = 10
  def description = "Find the sum of all the primes below two million."

  def run = Primes.primes.takeWhile(_ < 2000000).sum

  
}