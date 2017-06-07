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
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (0, _) => 1
    case (_, 0) => 1
    case (c, r) if c == r => 1
    case (_, _) => pascal(c - 1, r - 1) + pascal(c, r-1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def loop(chars: List[Char], acc: Int): Boolean = (chars, acc) match {
      case (c, 0) if c.isEmpty => true
      case (c, _) if c.isEmpty => false
      case (_, a) if a < 0 => false
      case (c, m) => c match {
        case '(' :: tail => loop(tail, acc + 1)
        case ')' :: tail => loop(tail, acc - 1)
        case _ :: tail => loop(tail, acc)
      }
    }
    loop(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
    case (0, _)  => 1
    case (m, _) if m < 0 => 0
    case (_, c) if c.isEmpty => 0
    case (m, c) => countChange(m - c.head, c) + countChange(m, c.tail)
  }

}