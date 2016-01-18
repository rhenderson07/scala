package problems

import common.math.MyMath

object Problem088 extends Problem with App {
  def number = 88
  def description = "What is the sum of all the minimal product-sum numbers for 2 =< k =< 12000?"
  def run = getMinSumProducts(target).values.toSet.sum

  val target = 12000

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

  /**
   * Return all factorization with a product less than or equal to the specified max value.
   */
  def getFactorizations(maxVal: Int) = {
    // cache factorizations for each number of terms
    val termsCache = scala.collection.mutable.Map[Int, Seq[List[Int]]]()

    def retrieveFactorizations(terms: Int): Seq[List[Int]] = {
      termsCache.getOrElseUpdate(terms, calculate(terms))
    }

    def calculate(terms: Int) = {
      if (terms == 1) (2 to maxVal).map(List(_)).toList
      else {
        for {
          factors <- retrieveFactorizations(terms - 1)
          cand <- factors.head to (maxVal / factors.product)
          if (cand >= factors.head)
        } yield cand :: factors
      }
    }

    // find all factorizations
    val maxTerms = (math.log(maxVal) / math.log(2)).toInt
    (2 to maxTerms).flatMap(retrieveFactorizations(_).reverse)
  }

  def isSumProduct(l: List[Int]) = {
    l.sum == l.product
  }

  /**
   * Calculate to total number of factors that would result from converting this list into a sumProduct list.
   */
  def sumProductFactorCount(factors: List[Int]): Int = factors.size + factors.product - factors.sum

  /**
   * Return mapping of min sum products for each k between 2 and maxK
   */
  def getMinSumProducts(maxK: Int) = {
    val maxNumber = 2 * maxK

    // group factorizations by k
    lazy val factorizations = getFactorizations(maxNumber)
    lazy val kGroups = factorizations.filter(x => x.sum <= x.product).groupBy(sumProductFactorCount)

    // for each group, find min sumProduct
    lazy val minSumProds = for {
      group <- kGroups
      val (k, factorLists) = group
      val minProdSum = factorLists.map(_.product).min
      if (k <= maxK)
    } yield (k, minProdSum)

    minSumProds.toMap
  }

  def minProductSum(maxK: Int) = getMinSumProducts(maxK).values.toSet.sum

  time(minProductSum)(target)
}