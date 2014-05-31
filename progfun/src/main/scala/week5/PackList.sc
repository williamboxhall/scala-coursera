def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: _ =>
    val (matches, rest) = xs.span(_ == x)
    matches :: pack(rest)
}

pack(List("a", "a", "a", "b", "c", "c", "a"))
// should be List(List("a", "a", "a"), List("b"), List("c", "c"), List("a")).


def encode[T](xs: List[T]): List[(T, Int)] = xs match {
  case Nil => Nil
  case x :: _ =>
    val (matches, rest) = xs.span(_ == x)
    (x, matches.length) :: encode(rest)
}

encode(List("a", "a", "a", "b", "c", "c", "a"))
// should be List(("a", 3), ("b", 1), ("c", 2), ("a", 1)).
