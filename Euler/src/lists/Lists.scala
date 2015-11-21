package example

object Lists {

  def sum(xs: List[Int]): Int = {
    return if(xs.isEmpty) 0 else xs.head + sum(xs.tail)
  }

  def max(xs: List[Int]): Int = {
    return xs.reduce((x,y) => if (x > y) x else y)
  }
}
