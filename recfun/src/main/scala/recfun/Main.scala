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
    def balance(numClosesNeeded: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) {
        return numClosesNeeded == 0
      }

      val head = chars.head

      if (head == '(') {
        balance(numClosesNeeded + 1, chars.tail)
      } else if (head == ')') {
        if (numClosesNeeded == 0) {
          false
        } else {
          balance(numClosesNeeded - 1, chars.tail)
        }
      } else {
        balance(numClosesNeeded, chars.tail)
      }
    }
    balance(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
