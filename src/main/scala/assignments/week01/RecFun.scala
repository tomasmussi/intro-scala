package assignments.week01

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   * Write a recursive function which verifies the balancing of parentheses in a string, which we represent as a
   * List[Char] not a String. For example, the function should return true for the following strings:
   * (if (zero? x) max (/ 1 x))
   * I told him (that it’s not (yet) done). (But he wasn’t listening)
   * The function should return false for the following strings:
   * :-)
   * ())(
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(current: Char, chars: List[Char], count: Int): Int = {
      if (count < 0 ) count
      else {
        if (chars.isEmpty) {
          if (current == '(' ) count + 1
          else if (current == ')') count - 1
          else count
        } else {
          if (current == '(' ) {
            balanceIter(chars.head, chars.tail, count + 1)
          } else if (current == ')') {
            balanceIter(chars.head, chars.tail, count - 1)
          } else {
            balanceIter(chars.head, chars.tail, count)
          }
        }
      }
    }
    balanceIter(chars.head, chars.tail, 0) == 0
  }

  /**
   * Exercise 3
   * Write a recursive function that counts how many different ways you can make change for an amount,
   * given a list of coin denominations. For example, there are 3 ways to give change for 4 if you have coins
   * with denomination 1 and 2: 1+1+1+1, 1+1+2, 2+2.
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countChangeIter(money: Int, coins:List[Int]): Int = {
      if (money == 0)  1
      else if (money < 0) 0
      else if (coins.isEmpty && money > 0) 0
      else {
        countChangeIter(money - coins.head, coins ) + // Discount one time a coin
          countChangeIter(money, coins.tail) // No more coins of this size
      }
    }
    countChangeIter(money, coins.sorted(Ordering.Int.reverse))
  }
}
