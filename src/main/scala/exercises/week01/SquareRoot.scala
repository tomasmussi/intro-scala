package exercises.week01

object SquareRoot {
  def abs(x:Double) = if (x < 0) -x else x

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  def isGoodEnough(guess: Double, x: Double) = {
    abs(guess * guess - x) / x < 0.001
  }

  def improve(guess: Double, x: Double) =(guess + x / guess) / 2

  def sqrt(x: Double) = sqrtIter(1, x)

  def main(args: Array[String]) = {
    println(sqrt(2))
    println(sqrt(16))

    println(sqrt(1e-6))
    println(sqrt(1e60))
  }

}
