package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {

  import Main.countChange

  test("countChange: no ways to count change for zero CHF") {
    countChange(1, Nil) === 0
  }

  test("countChange: one way to count change for zero amount and zero CHF") {
    countChange(0, Nil) === 1 // special case
  }

  test("countChange: one way to count change for zero amount and non-zero CHF") {
    countChange(0, List(1, 2)) === 1 // special case
  }

  test("countChange: single coin matching money") {
    assert(countChange(1, List(1)) === 1) // 1
  }


  test("countChange: minimalist positive example") {
    assert(countChange(3, List(1)) === 1) // 1,1,1
  }

  test("countChange: minimalist negative example") {
    assert(countChange(3, List(2)) === 0)
  }

  test("countChange: minimalist always include first denomination") {
    assert(countChange(3, List(1, 2)) === 2) // 1,1,1  1,2
  }

  test("countChange: example given in instructions") {
    assert(countChange(4, List(1, 2)) === 3) // 1,1,1,1  1,1,2  2,2
  }

  test("countChange: denominations don't individually divide in to amount") {
    assert(countChange(5, List(2, 3)) === 1) // 2,3
  }

  test("countChange: need multiple copies of multiple parts") {
    assert(countChange(11, List(1, 2, 3)) === 16)
  }

  test("countChange: medium sized example") {
    assert(countChange(5, List(1, 2, 3)) === 5) // 1,1,1,1,1  1,1,1,2  1,1,3  1,2,2  2,3
  }

  test("countChange: medium sized example non-divisible denominations") {
    assert(countChange(15, List(2, 4, 7)) === 3) // 2,2,2,2,7  2,2,4,7  4,4,7
  }

  test("countChange: sorted CHF") {
    assert(countChange(300, List(5, 10, 20, 50, 100, 200, 500)) === 1022)
  }

  test("countChange: no pennies") {
    assert(countChange(301, List(5, 10, 20, 50, 100, 200, 500)) === 0)
  }

  test("countChange: unsorted CHF") {
    assert(countChange(300, List(500, 5, 50, 100, 20, 200, 10)) === 1022)
  }

  test("countChange: large CHF") {
    assert(countChange(3, List.range(1, 100000)) === 3) // 1,1,1  1,2  3
  }

  test("countChange: duplicate items in CHF") {
    assert(countChange(2, List.fill(100000)(1)) === 1) // 1,1
  }
}
