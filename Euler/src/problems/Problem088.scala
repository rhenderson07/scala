package problems

/**
 * Math insight to help solve problem:
 *
 * The first insight we should get is that if we have a set of factors say {2,3,4} we get that 2*3*4 = 24
 * and 2+3+4 = 9. Something that isn't exactly a product-sum like we are looking for. However, we can add
 *  ones which doesn't change the product but change the sum. Indeed if we add 24-9 = 15 ones we will get
 *  a product sum with 18 factors. In other words any set of factors can be converted into a product-sum
 *  for k = "product of factor" - "sum of factors" + "number of factors". Whether this is minimal or not is
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
object Problem088 extends Problem with App {
  def number = 88
  def description = "What is the sum of all the minimal product-sum numbers for 2 =< k =< 12000?"
  def run = getMinSumProductNumbers(target).values.toSet.sum

  val target = 12000

  /**
   * Return all factorizations with a product less than or equal to the specified max value.
   */
  def getFactorizations(maxVal: Int) = {
    // cache factorizations for each number of terms
    lazy val termsCache = scala.collection.mutable.Map[Int, Seq[List[Int]]]()

    // calculate all factorizations for the given number of terms
    def calculate(terms: Int): Seq[List[Int]] = {
      if (terms == 1) (2 to maxVal).map(List(_)).toList
      else {
        for {
          factors <- retrieveFactorizations(terms - 1)
          cand <- factors.head to (maxVal / factors.product)
          if (cand >= factors.head)
        } yield cand :: factors
      }
    }

    def retrieveFactorizations(terms: Int) = {
      termsCache.getOrElseUpdate(terms, calculate(terms))
    }

    // find all factorizations
    lazy val maxTerms = (math.log(maxVal) / math.log(2)).toInt
    (2 to maxTerms).flatMap(retrieveFactorizations(_).reverse)

    // tried to find all factorizations using stream instead of log. Time increased by 40%
    // Stream.from(2).map(retrieveFactorizations(_).reverse).takeWhile(_.nonEmpty).flatten
  }

  /**
   * Calculate to total number of factors that would result from converting this list into a sum product list.
   */
  def sumProductFactorCount(factors: List[Int]): Int = factors.size + factors.product - factors.sum

  /**
   * Return mapping of minimum sum products for each k between 2 and maxK
   */
  def getMinSumProductNumbers(maxK: Int) = {
    // for all k values, 2 * k serves as the upper bound for a minimum sum product 
    lazy val maxNumber = 2 * maxK

    // group factorizations by k
    lazy val factorizations = getFactorizations(maxNumber)
    lazy val kGroups = factorizations.filter(x => x.sum <= x.product).groupBy(sumProductFactorCount)

    // for each group, find min sum product
    lazy val minSumProds = for {
      group <- kGroups
      val (k, factorLists) = group
      val minProdSum = factorLists.map(_.product).min
      if (k <= maxK)
    } yield (k, minProdSum)

    minSumProds.toMap
  }

  /**
   * Find the sum of all sum product numbers between 2 and the provided max k value.
   */
  def minSumProductNumberSum(maxK: Int): Long = getMinSumProductNumbers(maxK).values.map(_.longValue).toSet.sum

  // time process
  time(minSumProductNumberSum)(target)
}