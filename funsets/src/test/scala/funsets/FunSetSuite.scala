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

  test("intersect contains only common elements") {
    val s = intersect(union(singletonSet(1), singletonSet(2)), union(singletonSet(2), singletonSet(3)))
    assert(!contains(s, 1), "Intersect 1")
    assert(contains(s, 2), "Intersect 2")
    assert(!contains(s, 3), "Intersect 3")
  }

  test("diff contains the elements in the first set that don't exist in the second") {
    val s = diff(union(singletonSet(1), singletonSet(2)), union(singletonSet(2), singletonSet(3)))
    assert(contains(s, 1), "Diff 1")
    assert(!contains(s, 2), "Diff 2")
    assert(!contains(s, 3), "Diff 3")
  }

  test("filter retains from set those elements that match the predicate") {
    val oneTwoThreeFour = union(union(union(singletonSet(1), singletonSet(2)), singletonSet(3)), singletonSet(4))
    val evens = filter(oneTwoThreeFour, x => x % 2 == 0)

    assert(!contains(evens, 1), "Filter 1")
    assert(contains(evens, 2), "Filter 2")
    assert(!contains(evens, 3), "Filter 3")
    assert(contains(evens, 4), "Filter 4")
  }

  test("forAll is true when all match") {
    val s = union(union(union(singletonSet(1), singletonSet(2)), singletonSet(3)), singletonSet(4))
    assert(forall(s, _ < 5), "forAll all match")
  }

  test("forAll is false when one does not match") {
    val s = union(union(union(singletonSet(1), singletonSet(2)), singletonSet(3)), singletonSet(4))
    assert(!forall(s, _ < 4), "forAll one does not match")
  }

  test("forAll is false when none match") {
    val s = union(union(union(singletonSet(1), singletonSet(2)), singletonSet(3)), singletonSet(4))
    assert(!forall(s, _ > 4), "forAll none match")
  }

  test("exists is true when all match") {
    val s = union(union(union(singletonSet(1), singletonSet(2)), singletonSet(3)), singletonSet(4))
    assert(exists(s, _ < 5), "exists all match")
  }

  test("exists is true only one matches") {
    val s = union(union(union(singletonSet(1), singletonSet(2)), singletonSet(3)), singletonSet(4))
    assert(exists(s, _ == 1), "exists one match")
  }

  test("exists is false when none match") {
    val s = union(union(union(singletonSet(1), singletonSet(2)), singletonSet(3)), singletonSet(4))
    assert(!exists(s, _ > 4), "exists none match")
  }

  test("map changes all elements") {
    val s = map(union(singletonSet(1), singletonSet(2)), _ + 2)
    assert(!contains(s, 1))
    assert(!contains(s, 2))
    assert(contains(s, 3))
    assert(contains(s, 4))
  }
}
