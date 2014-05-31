package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("times") {
    new TestTrees {
      assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
    }
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
    new TestTrees {
      assert(singleton(List(Leaf('a', 1))))
      assert(singleton(List(t1)))
      assert(singleton(List(t2)))
      assert(!singleton(List(Leaf('a', 1), Leaf('v', 1))))
      assert(!singleton(List()))
    }
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("createCodeTree") {
    new TestTrees {
      assert(createCodeTree("abacba") ==
        Fork(Fork(Leaf('c', 1), Leaf('b', 2), List('c', 'b'), 3), Leaf('a', 3), List('c', 'b', 'a'), 6))
    }
  }

  test("decode") {
    println(createCodeTree("AAAAAAAABBBCDEFGH"))
    //assert(decode(createCodeTree("abacba"), List(1, 0, 1, 1)) == List('d'))
    //println(decode(createCodeTree("aaaaaaaaabbbcdefgh"), List(1, 0, 1, 1)))
    assert(decode(createCodeTree("AAAAAAAABBBCDEFGH"), List(1, 0, 1, 1)) == List('D'))
    //assert(decode(createCodeTree("aaaaaaaaabbbcdefgh"), List(1, 0, 0, 0, 1, 0, 1, 0)) == List('b', 'a', 'c'))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  //{A8,{{{G1,H1(GH)2},{E1,F1(EF)2}(GHEF)4},{{C1,D1(CD)2},B3(CDB)5}(GHEFCDB)9}(AGHEFCDB)17}



  //Fork(Fork(Fork(Fork(Leaf(C,1),Leaf(E,1),List(C, E),2),Leaf(G,1),List(C, E, G),3),Leaf(B,3),List(C, E, G, B),6),Fork(Fork(Fork(Leaf(D,1),Leaf(F,1),List(D, F),2),Leaf(H,1),List(D, F, H),3),Leaf(A,8),List(D, F, H, A),11),List(C, E, G, B, D, F, H, A),17)
}
