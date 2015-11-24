package problems

object Problem012 extends Problem with App {
  def number = 12
  def description = "What is the value of the first triangle number to have over five hundred divisors?"

  def run = -1

  
  
  lazy val triangles: Stream[Long] = Stream.from(1).map(x => (1L to x.longValue()).fold(0L)(_+_))
  
  def triangleAt(n: Int): Long = triangles.take(n).last
  

  triangles.take(10).foreach(println)
  
}