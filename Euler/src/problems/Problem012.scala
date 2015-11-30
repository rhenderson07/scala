package problems

import common.MyMath

object Problem012 extends Problem with App {
  def number = 12
  def description = "What is the value of the first triangle number to have over five hundred divisors?"

  lazy val run = firstTriangleWithMinDivisors2(500)._1._2

  // first attempt, using range to calculate a sum at each item in the stream. Very slow
  lazy val triangles: Stream[Long] = Stream.from(1).map(x => (1L to x.longValue()).fold(0L)(_ + _))

  // attempt to reuse stream. runs slightly faster than triangle 1, still does not terminate for value 100,000
  lazy val triangles2: Stream[Long] = 1L #:: Stream.from(2).map(x => triangles2(x - 2) + x)

  // attempt with iteration. Blazingly fast. Runs out of heap space for value 10,000,000
  def incrementTriangle(pair: (Int, Long)) = (pair._1 + 1, pair._1 + 1 + pair._2)
  lazy val trianglePairs: Stream[(Int, Long)] = Stream.iterate((1, 1L))(incrementTriangle)
  lazy val triangles3: Stream[Long] = trianglePairs.map(_._2)

  //fourth attempt, using scan left
  lazy val triangles4 = Stream.from(2).map(_.longValue).toIterator.scanLeft(1L)(_ + _)

  // first attempt. takes too long to find divisors. does not terminate
  def firstTriangleWithMinDivisors(minDivisors: Int): (Int, Long) = {
    trianglePairs.find(x => MyMath.findDivisors(x._2).length > minDivisors).get
  }

  // second attempt. Uses prime method to count divisors. Runs instantly
  def firstTriangleWithMinDivisors2(minDivisors: Int) = {
    val triangleDivisors = trianglePairs.map(x => MyMath.divisorCount(x._2))
    trianglePairs.zip(triangleDivisors).find(x => x._2 > minDivisors).get
  }

  // Third attempt. Uses scanLeft to find triangle numbers. Uses prime method to count divisors.
  def firstTriangleWithMinDivisors3(minDivisors: Int) = {
  }

  println(run)
}