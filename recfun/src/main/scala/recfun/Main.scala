package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def closesNeededFor(char: Char): Int = {
      char match {
        case '(' => 1
        case ')' => -1
        case _ => 0
      }
    }
    def balance(numClosesNeeded: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) {
        return numClosesNeeded == 0
      }
      if (chars.head == ')' && numClosesNeeded == 0) {
        return false
      }

      balance(numClosesNeeded + closesNeededFor(chars.head), chars.tail)
    }
    balance(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      1
    }
    if (coins.isEmpty) {
      0
    }
    5
  }
}
