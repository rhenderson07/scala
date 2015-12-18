package common.upgrades

class MyFunction[A, B](funct: A => B) {
  // Alternate Memo method defined at http://stackoverflow.com/questions/25129721/scala-memoization-how-does-this-scala-memo-work

  // general technique for converting a function to a memoized function
  def memoize = new (A => B) {
    val cache = scala.collection.mutable.Map[A, B]()
    def apply(x: A): B = {
      cache.getOrElseUpdate(x, funct(x))
    }
  }

}