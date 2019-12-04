package exercises.week02

import math.abs

/**
 * Lecture 2.3
 * */
object FixedPoint {

  val tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double) = abs( (x -y) / x ) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) return next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  def sqrt(x: Double) = {
    fixedPoint(averageDamp(y => x / y))(1)
  }

  def main(args: Array[String]) {
    println("Fixed point: " + fixedPoint(x => 1 + x / 2)(1))
    println("Fixed point Square root of 2: " + fixedPoint(y => (y + 2 / y) / 2)(1))
    println("Fixed point Square root of 4: " + fixedPoint(y => (y + 4 / y) / 2)(1))

    println("Square root of 2: " + sqrt(2))
    println("Square root of 4: " + sqrt(4))
  }


}
