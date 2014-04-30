package recfun

import collection.SortedSet

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

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

  def countChange(money: Int, coins: List[Int]): Int = {
    def countChange(money: Int, coins: SortedSet[Int]): Int = {
      if (coins.isEmpty || coins.head > money) 0
      else if (coins.head == money) 1
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
    countChange(money, coins.to[SortedSet])
  }
}
