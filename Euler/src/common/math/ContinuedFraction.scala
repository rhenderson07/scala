package common.math

class ContinuedFraction(val wholePart: Int, val repeatedBlock: List[Int]) {
    val period = repeatedBlock.length

    override def toString(): String = {
      s"[$wholePart; (${repeatedBlock.mkString(",")})]"
    }
  }