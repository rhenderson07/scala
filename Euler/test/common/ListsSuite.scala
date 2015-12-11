package common

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Lists._
import com.sun.org.apache.xpath.internal.operations.NotEquals
import scala.collection.immutable.LinearSeq

@RunWith(classOf[JUnitRunner])
class ListsSuite extends FunSuite {

  def targetFunction[T <: Any](property: (T, T) => Boolean)(candList: LinearSeq[T]): Boolean = Lists.holdsForFamily(property)(candList)

  def equals[T <: Any](t1: T, t2: T) = t1.equals(t2)
  def notEqual[T <: Any](t1: T, t2: T) = !t1.equals(t2)
  def lessThan[T <: Int](t1: T, t2: T) = t1 < t2

  test("Empty family returns true") {
    assert(targetFunction(equals)(List()))
  }

  test("List size 1 returns true") {
    assert(targetFunction(equals)(List(1)))
  }

  test("1_1 returns true") {
    assert(targetFunction(equals)(List(1, 1)))
  }

  test("1,2 returns false") {
    assert(targetFunction(equals)(List(1, 2)) === false)
  }

  test("Does not compare item to self") {
    assert(targetFunction(notEqual)(List(1)) === true)
  }

  test("True when property applies to all items") {
    assert(targetFunction(notEqual)(List(1, 2, 3, 4)) === true)
  }

  test("True when property is not reflective") {
    assert(targetFunction(lessThan)(List(1, 2, 3, 4)) === true)
  }
}
