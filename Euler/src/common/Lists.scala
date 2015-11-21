package common

object Lists {
  /**
   * accepts List of Ints. multiplies all elements in the List to produce a Long
   */
  def product(nums: List[Int]): Long = {
    if (nums.isEmpty)
      1
    else
      nums.head * product(nums.tail)
  }

  /**
   * accepts List of BigInts. multiplies all elements in the List to produce a BigInt
   */
  def product(nums: List[BigInt]): BigInt = {
    if (nums.isEmpty)
      1
    else
      nums.head * product(nums.tail)
  }
}