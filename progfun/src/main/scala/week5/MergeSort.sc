def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}

def merge[T](xs: List[T], ys: List[T])(implicit ord: Ordering[T]): List[T] =
  (xs, ys) match {
    case (Nil, zs) => zs
    case (zs, Nil) => zs
    case (x :: xs1, y :: ys1) =>
      if (ord.lt(x,y)) x :: merge(xs1, ys)
      else y :: merge(xs, ys1)
  }

msort(List(3, 1, 7, 3, 6, 2, 8))
msort(List("apple", "pineapple", "banana"))