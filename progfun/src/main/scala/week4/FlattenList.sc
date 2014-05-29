def flatten(xs: List[Any]): List[Any] = xs match {
  case List() => List()
  case y :: ys => y match {
    case n: Int => n :: flatten(ys)
    case n: List[Any] => flatten(n) ::: flatten(ys)
  }
}

flatten(List(List(1, 1), 2, List(3, List(5, 8))))














