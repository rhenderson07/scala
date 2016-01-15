package problems

import common.math.MyMath

object Problem088 extends Problem with App {
  def number = 88
  def description = "What is the sum of all the minimal product-sum numbers for 2 =< k =< 12000?"
  def run = -1L

  /*
   * Math insight to help solve problem:
   * 
   * The first insight we should get is that if we have a set of factors say {2,3,4} we get that 2*3*4 = 24
   * and 2+3+4 = 9. Something that isn’t exactly a product-sum like we are looking for. However, we can add
   *  ones which doesn’t change the product but change the sum. Indeed if we add 24-9 = 15 ones we will get
   *  a product sum with 18 factors. In other words any set of factors can be converted into a product-sum
   *  for k = “product of factor” – “sum of factors” + “number of factors”. Whether this is minimal or not is
   *  another question which we wont answer just yet.
   *  
   *  The second insight we might get is that the minimal product-sum for k is larger than or equal to k
   *  since the minimal product-sum consist of k ones which sums to k. The upper limit for the minimal
   *  product sum for k is 2k. The reason is that we can always use the factors {2,k} this gives us the
   *  product 2k and the sum 2+k. By adding k-2 ones we end up with a valid product sum for k which equals
   *  2k. In other words the minimal product-sum (mps) for k is k =< mps(k) =< 2k.
   *  
   *  With these two insight we should be able to find the solution for the problem.
   */

  // second attempt. using insights
  {
    val maxK = 12000
    val maxNumber = 2 * maxK

    val numFactors = (Math.log(maxNumber) / Math.log(2)).intValue
    val factors = Array.fill(numFactors)(0)

    val k = (0 to maxK).map(x => x * 2).toArray;
    k(1) = 0;

    factors(0) = 1;
    val curMaxFactor = 1;
    val j = 0;

    /**
     * Return all factorization with a product less than or equal to the specified max value.
     */
    def getFactorizations(maxVal: Int) = {
      // cache factorizations for each number of terms
      val termsCache = scala.collection.mutable.Map[Int, Seq[List[Int]]]()

      def getFactorizations(terms: Int): Seq[List[Int]] = {
        termsCache.getOrElseUpdate(terms, calculate(terms))
      }

      def calculate(terms: Int) = {
        if (terms == 1) (2 to maxVal).map(List(_)).toList
        else {
          for {
            factors <- getFactorizations(terms - 1)
            cand <- factors.head to (maxVal / factors.product)
            if (cand >= factors.head)
          } yield cand :: factors
        }
      }

      // find all factorizations
      val maxTerms = (math.log(maxVal) / math.log(2)).toInt
      (2 to maxTerms).flatMap(getFactorizations(_).reverse)
    }

    def isSumProduct(l: List[Int]) = {
      l.sum == l.product
    }

    def getMinSumProducts(maxK: Int): List[Int] = {
      val maxNumber = 2 * maxK
      val factorizations = getFactorizations(maxNumber)
      
      

      // find min prodSum for len 2
      def findMin(len: Int) = factorizations.find(isSumProduct(_)).get.sum
      
      (2 to maxK).map(findMin).toList

      //      (2 to maxK).map(factorizations.find(isSumProduct).get).sum
      //      factorizations.withFilter(_.size == 2)
      //
      //      -1L
    }

    println("starting")
    //    getFactorizations(50000).withFilter(_.size >= 12).foreach(println)
    println(getMinSumProducts(4))
    println("done")
  }

  //first attempt. Very clunky
  {
    /**
     * For given value k, returns minimal sum product composed of that many elements
     */
    def minimalSumProduct(k: Int): Int = {
      Stream.from(1).map(i => sumProduct(k)(i)).find(_.nonEmpty).get.get.sum
    }

    def isSumProduct(l: List[Int]) = {
      l.sum == l.product
    }

    /**
     * For the given value x, find all sum product sets
     */
    def sumProduct(length: Int)(x: Int): Option[List[Int]] = {
      // get divisors in reverse order
      val divisors = MyMath.findDivisors(x).sorted.map(_.toInt)
      comb(length, divisors).find(isSumProduct)
    }

    def combInner[T](n: Int, l: List[T]): List[List[T]] =
      n match {
        case 0 => List(List())
        case _ => for (
          element <- l;
          sl <- combInner(n - 1, l.dropWhile(_ != element))
        ) yield element :: sl
      }

    def comb[T](n: Int, l: List[T]): List[List[T]] = combInner(n, l.distinct)

    //  MyMath.findDivisors(156).sorted.reverse.combinations(3).foreach(println)
    //  sumProduct(2)(2).foreach(println)
    //println(minimalSumProduct(6))
    println
  }
}