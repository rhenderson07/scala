package common.upgrades

class MyString(target: String) {
  def isPalindrome: Boolean = target.equals(target.reverse)
}