package problems

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import common.upgrades.Implicits._

@RunWith(classOf[JUnitRunner])
class Problem076Suite extends FunSuite {
  
  test("Summation count accurate for input 5") {    
    assert(Problem076.summations(5) === 6)
  }
}
