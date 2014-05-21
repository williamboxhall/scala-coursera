abstract class IntSet {
  def incl(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  override def union(other: IntSet): IntSet = other

  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  override def union(other: IntSet): IntSet = {
    ((left union right) union other) incl elem
  }

  override def toString = "{" + left + elem + right + "}"
}
val foo = Empty.incl(1).incl(2).incl(3)
val bar = Empty.incl(4).incl(5).incl(6)
val baz = Empty.union(Empty.incl(1))
val maz = Empty.incl(2).union(Empty)
val quux = new NonEmpty(2, Empty, Empty).union(Empty)
val starbuux = Empty.incl(1).union(Empty.incl(2))
val alcatraz: IntSet = foo.union(bar)


