List("a", "b", "c", "d").foldLeft("")("" + _ + _)
List("a", "b", "c", "d").foldRight("")("" + _ + _)

def concat[T](xs: List[T], ys: List[T]): List[T] =
  (xs foldRight ys)(_ :: _)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())((x, z) => f(x) :: z)

mapFun(List("a", "b", "c", "d"), (x:String) => "1" + _)
// should be List("1a", "1b", "1c", "1d")

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((_, length: Int) => length + 1)

lengthFun(List("a", "b", "c", "d"))
// should be 4