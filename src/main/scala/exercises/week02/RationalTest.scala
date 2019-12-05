package exercises.week02

object RationalTest {

  def useRational = {
    val x = new Rational(2, 3)
    val y = new Rational(1, 3)
    println(x.add(y))
    println(x.neg)
    println(x.sub(y))

    println(y.less(x))
    println(y.max(x))

    println(x.max(x.neg))

    try {
      val strange = new Rational(1, 0)
    } catch {
      case x: IllegalArgumentException => {
        println("ups")
      }
    }
    println(new Rational(2))
    // Infix operation
    println(x add y)
  }

  def useImprovedRational: Unit = {
    val x = new ImprovedRational(2, 3)
    val y = new ImprovedRational(1, 3)
    println(x + y)
    println(-x)
    println(x - y)

    println(y < x)
    println(y.max(x))

    println(x.max(-x))

    try {
      val strange = new ImprovedRational(1, 0)
    } catch {
      case x: IllegalArgumentException => {
        println("ups")
      }
    }
    println(new ImprovedRational(2))
    // Infix operation
    println(x + y)
  }

  def main(args: Array[String]): Unit = {
    useRational
    useImprovedRational
  }
  /**
   * Table of precedence in operators. The precedence is determined by its first character
   * The following table lists the characters in increasing order of priority precedence:
   *    (all leters)
   *    |
   *    ^
   *    &
   *    < >
   *    = !
   *    :
   *    + -
   *    * / %
   *    (all other special characters)
   *
   * */
  /**
   * For example, the next expression, considering a, b, c and d as values:
   *    a + b ^? c ?^ d less a ==> b | c
   * is reduced to:
   *    ( ( (a + b) ^? (c ?^ d) ) less ( (a ==> b) | c) )
   * */
}

