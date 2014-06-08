def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] =
    if (k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens
  def isSafe(col: Int, queens: List[Int]) = {
    !queens.contains(col) && safeDiag(col, queens, -1) && safeDiag(col, queens, +1)
  }
  def safeDiag(col: Int, queens: List[Int], dir: Int): Boolean = queens match {
      case Nil => true
      case q :: qs => (q != col + dir) && safeDiag(col + dir, qs, dir)
    }
  placeQueens(n)
}

queens(1)
queens(2)
queens(4)
queens(8)

