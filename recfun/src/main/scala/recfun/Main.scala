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
      if (chars.isEmpty) numClosesNeeded == 0
      else if (chars.head == ')' && numClosesNeeded == 0) false
      else balance(numClosesNeeded + closesNeededFor(chars.head), chars.tail)
    }
    balance(0, chars)
  }

  //1,1,1,1   1,1,2   2,2

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      return 1
    }
    if (coins.isEmpty || coins.head > money) {
      return 0
    }

    val denom = coins.head

    // 1,1,1,1
    // 1,1,2
    // 1,2,1 <wraong>
    // 2,2

    // recurse on smallest number till over the top. if finished, round 2
    var count = 0;
    //var solutions = Set[Int]



    7
  }
}
