package common.upgrades

/**
 * This is the "Pimp My Library pattern," defined by Odersky.
 */
object Implicits {
  implicit def stringToMyString[T](s: String) = new MyString(s)
  implicit def listToMyTraversable[T](l: Traversable[T]) = new MyTraversable(l)
  implicit def listToMyList[T](l: List[T]) = new MyList(l)
}