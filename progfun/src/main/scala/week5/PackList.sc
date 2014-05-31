def pack[T](list: List[T]): List[List[T]] = list match {
  case Nil => Nil
  case x :: xs =>
    val span: (List[T], List[T]) = xs.span(_ == x)
    (x :: span._1) :: pack(span._2)
}

pack(List("a", "a", "a", "b", "c", "c", "a"))
// should be List(List("a", "a", "a"), List("b"), List("c", "c"), List("a")).

