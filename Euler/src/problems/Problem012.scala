package problems

import common.MyMath

object Problem012 extends Problem with App {
  def number = 12
  def description = "What is the value of the first triangle number to have over five hundred divisors?"

  def run = firstTriangleWithMinDivisors2(500)._1._2

  // first attempt, using range to calculate a sum at each item in the stream. Very slow
  lazy val triangles: Stream[Long] = Stream.from(1).map(x => (1L to x.longValue()).fold(0L)(_ + _))

  // attempt to reuse stream. runs slightly faster than triangle 1, still does terminate for value 100000
  val triangles2: Stream[Long] = 1L #:: Stream.from(2).map(x => triangles2(x - 2) + x)

  // attempt with iteration. Blazingly fast. still runs out of memory for value 10,000,000
  def incrementTriangle(pair: (Int, Long)) = (pair._1 + 1, pair._1 + 1 + pair._2)
  lazy val trianglePairs: Stream[(Int, Long)] = Stream.iterate((1, 1L))(incrementTriangle)
  lazy val triangles3: Stream[Long] = trianglePairs.map(_._2)

  // TODO attempt with scanLeft

  def firstTriangleWithMinDivisors(minDivisors: Int): (Int, Long) = {
    trianglePairs.find(x => MyMath.findDivisors(x._2).length > minDivisors).get
  }

  def firstTriangleWithMinDivisors2(minDivisors: Int) = {
    val triangleDivisors = trianglePairs.map(x => MyMath.findDivisors(x._2))
    trianglePairs.zip(triangleDivisors).find(x => x._2.length > minDivisors).get
  }
  
  // find factors
//  MyMath.primeDivisors(20).foreach(println)
//  println(MyMath.factorCount(1000000)) // slow
//  println(MyMath.findDivisors(1000000)) // fast
  
  // first number with 500 divisors
  //println(Stream.from(1).map(x=>(x,MyMath.findDivisors(x))).find(_._2.length > 500).get)
  
  // find Max divisors in all ints
  println((1 to 1000).map(x=>(x,MyMath.primeDivisors(x))).maxBy(_._2.length))

  // trianglePairs.take(10).foreach(println)
  // trianglePairs.take(5).foreach(println)
  // println(triangles2(100000))
  //println(firstTriangleWithMinDivisors2(500))
}