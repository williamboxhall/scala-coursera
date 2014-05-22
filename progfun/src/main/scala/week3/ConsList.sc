trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]

  def nth(n: Int): T
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false

  override def nth(n: Int): T = if (n == 0) head else tail.nth(n - 1)
}

class Nil[T] extends List[T] {
  def isEmpty = true

  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")

  override def nth(n: Int): T = throw new IndexOutOfBoundsException(s"$n")
}

def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
val foo = singleton(1)
foo.isEmpty
foo.head

val bar = new Cons(4, new Cons(3, new Cons(2, foo)))
bar.head

bar.nth(0)
bar.nth(1)
bar.nth(2)
bar.nth(3)
bar.nth(4)
