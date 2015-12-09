package common

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Lists._
import com.sun.org.apache.xpath.internal.operations.NotEquals

@RunWith(classOf[JUnitRunner])
class ListsSuite extends FunSuite {

  def equals[T <: Any](t1: T, t2: T) = t1.equals(t2)

  def notEqual[T <: Any](t1: T, t2: T) = !t1.equals(t2)

  test("Empty family returns true") {
    assert(holdsForFamily(equals)(Stream.Empty))
  }

  test("List size 1 returns true") {
    assert(holdsForFamily(equals)(Stream(1)))
  }

  test("1_1 returns true") {
    assert(holdsForFamily(equals)(Stream(1, 1)))
  }

  test("1,2 returns false") {
    assert(holdsForFamily(equals)(Stream(1, 2)) === false)
  }

  test("Does not compare item to self") {
    assert(holdsForFamily(notEqual)(Stream(1)) === true)
  }

  test("True when property applies to all items") {
    assert(holdsForFamily(notEqual)(Stream(1, 2, 3, 4)) === true)
  }
}
