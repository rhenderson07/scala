
package problems

object Problem044 extends Problem with App {
  def number = 44
  def description = "Find the pair of pentagonal numbers, Pj and Pk, for which their sum and difference are pentagonal and D = |Pk − Pj| is minimised; what is the value of D?"

  lazy val run = -1L

  lazy val pentagonal = Stream.from(1).map(n => n * (3 * n - 1) / 2)
  
//  def 

  //println(run)
  println(pentagonal(1))
}