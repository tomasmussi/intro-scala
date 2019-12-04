package exercises.week01

import scala.annotation.tailrec

object TailRecursion {

  def factorial(n: Int): Int = {
    if (n == 1) n
    else n * factorial(n -1)
  }

  @tailrec
  def tailfactorial(n: Int, result: Int = 1): Int = {
    if (n == 1) result
    else tailfactorial(n - 1, result * n)
  }

  def main(args: Array[String]) = {
    println(factorial(6))
    println(factorial(5))
    println(factorial(4))

    println(tailfactorial(6))
    println(tailfactorial(5))
    println(tailfactorial(4))
  }

}
