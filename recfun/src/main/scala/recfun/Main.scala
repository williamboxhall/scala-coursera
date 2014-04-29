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
    def deltaFor(char: Char): Int = {
      char match {
        case '(' => 1
        case ')' => -1
        case _ => 0
      }
    }
    def balance(depth: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) depth == 0
      else if (chars.head == ')' && depth == 0) false
      else balance(depth + deltaFor(chars.head), chars.tail)
    }
    balance(0, chars)
  }

  //1,1,1,1   1,1,2   2,2

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChange(runningTotal: Int, money: Int, coins: List[Int], sequence: List[Int]): Int = {
      if (runningTotal > money || coins.isEmpty) 0
      else {
        if (runningTotal + coins.head == money) {
          val successfulSequence = sequence :+ coins.head
          println(s"Found match on $successfulSequence")
          1
        } else {
          countChange(runningTotal + coins.head, money, coins, sequence :+ coins.head) +
            //countChange(runningTotal + coins.head, money, coins.tail, sequence :+ coins.head) +
            countChange(runningTotal, money, coins.tail, sequence)

        }
      }
    }
    countChange(0, money, coins.sorted, Nil)
  }


}
