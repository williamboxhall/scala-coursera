def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (for (pairs <- xs zip ys) yield pairs._1 * pairs._2).sum

scalarProduct(Vector(1, 2), Vector(3, 4))