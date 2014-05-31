def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: _ =>
    val (matches, rest) = xs.span(_ == x)
    matches :: pack(rest)
}

pack(List("a", "a", "a", "b", "c", "c", "a"))
// should be List(List("a", "a", "a"), List("b"), List("c", "c"), List("a")).

