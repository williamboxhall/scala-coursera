package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
  val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  val treeFromExample = createCodeTree("AAAAAAAABBBCDEFGH")

  test("weight of a larger tree") {
    assert(weight(t1) === 5)
  }

  test("chars of a larger tree") {
    assert(chars(t2) === List('a', 'b', 'd'))
  }

  test("times") {
    assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("makeOrderedLeafList honours reverse alphabetical order where possible (after weights)") {
    assert(makeOrderedLeafList(List(('c', 1), ('b', 2), ('a', 1))) === List(Leaf('c', 1), Leaf('a', 1), Leaf('b', 2)))
  }

  test("singleton") {
    assert(singleton(List(Leaf('a', 1))))
    assert(singleton(List(t1)))
    assert(singleton(List(t2)))
    assert(!singleton(List(Leaf('a', 1), Leaf('v', 1))))
    assert(!singleton(List()))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("createCodeTree") {
    assert(createCodeTree("abacba") ===
      Fork(Leaf('a', 3), Fork(Leaf('b', 2), Leaf('c', 1), List('b', 'c'), 3), List('a', 'b', 'c'), 6))
  }

  test("decode") {
    assert(decode(treeFromExample, List(0)) === List('A'))
    assert(decode(treeFromExample, List(1, 0, 1, 1)) === List('D'))
    assert(decode(treeFromExample, List(1, 0, 0, 0, 1, 0, 1, 0)) === List('B', 'A', 'C'))
  }

  test("decoded secret") {
    assert(decodedSecret.foldLeft("")(_ + _) === "huffmanestcool")
  }

  test("encode") {
    assert(encode(treeFromExample)(List('A')) === List(0))
    assert(encode(treeFromExample)(List('D')) === List(1, 0, 1, 1))
    assert(encode(treeFromExample)(List('B', 'A', 'C')) === List(1, 0, 0, 0, 1, 0, 1, 0))
  }

  test("quickEncode") {
    assert(quickEncode(treeFromExample)(List('A')) === List(0))
    assert(quickEncode(treeFromExample)(List('D')) === List(1, 0, 1, 1))
    assert(quickEncode(treeFromExample)(List('B', 'A', 'C')) === List(1, 0, 0, 0, 1, 0, 1, 0))
  }

  test("decode and encode a very short text should be identity") {
    assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
  }
}
