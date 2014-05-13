package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import FunSets._

@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {
  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  test("singletonSet(1) contains 1") {
    assert(contains(singletonSet(1), 1), "Singleton")
  }

  test("singletonSet(1) does not contain 2") {
    assert(!contains(singletonSet(1), 2), "Singleton")
  }

  test("singletonSet(2) contains 2") {
    assert(contains(singletonSet(2), 2), "Singleton")
  }

  test("singletonSet(2) does not contain 1") {
    assert(!contains(singletonSet(2), 1), "Singleton")
  }

  test("singletonSet(3) contains 3") {
    assert(contains(singletonSet(3), 3), "Singleton")
  }

  test("union contains all elements") {
    val s = union(singletonSet(1), singletonSet(2))
    assert(contains(s, 1), "Union 1")
    assert(contains(s, 2), "Union 2")
    assert(!contains(s, 3), "Union 3")
  }
}
