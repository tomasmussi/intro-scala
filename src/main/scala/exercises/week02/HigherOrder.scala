package exercises.week02

import scala.annotation.tailrec

object HigherOrder {
  def sum(f: Int => Int, a: Int, b: Int): Int = {

    @tailrec
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  }

  /*
    // My not so elegant solution with auxiliary function
    def product(f: Int => Int)(a: Int, b: Int) : Int = {
      def aux(accum: Int, a: Int): Int = {
        if (a > b) accum
        else aux(accum * f(a), a + 1)
      }
      aux(1, a)
    }
   */

  def product(f: Int => Int)(a: Int, b: Int) : Int = {
    if (a > b) 1
    else f(a) * product(f) (a + 1, b)
  }

  def factorial(n: Int): Int = product(x => x)(1, n)

  /**
   * My 'MapReduce' function as asked in course
   * */
  def compute(op: (Int, Int) => Int, nullValue: Int)(f: Int => Int)(a: Int, b:Int): Int = {
    if (a > b) nullValue
    else op(f(a), compute(op, nullValue) (f) (a + 1, b))
  }

  /**
   * Combine function as defined in course
   * */
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int) (a: Int, b: Int): Int = {
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
  }


  def main(args: Array[String]) {
    println(sum(x => x, 1, 4))
    println(sum(x => x * x, 1, 4))

    println(product(x => x) (1, 4) )
    println(product(x => x * x) (1, 4))


    println(factorial(4))
    println(factorial(5))
    println()
    println(sum(x => x, 1, 4))
    println(compute((x,y) => x + y, 0)(x => x)(1,4))
    println()
    println(product(x => x) (1, 4) )
    println(compute((x,y) => x * y, 1)(x => x)(1,4))
    println(mapReduce(x => x, (x,y) => x * y, 1)(1,4))
  }

}
