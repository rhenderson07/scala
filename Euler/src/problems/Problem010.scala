package problems

import common.math.Primes

object Problem010 extends Problem with App {
  def number = 10
  def description = "Find the sum of all the primes below two million."

  def run = Primes.stream.takeWhile(_ < 2000000).sum

  
}