def isort(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case Nil => List(x)
  case y :: ys => if (x < y) x :: xs else y :: insert(x, ys)
}

isort(1 :: 4 :: 3 :: 9 :: 5 :: Nil)







