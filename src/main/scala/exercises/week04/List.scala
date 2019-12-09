package exercises.week04

import exercises.week03._

/**
 * Functions as objects, this is the implicit conversion that Scala does with functions.
 * */
object List {
  def apply[T](x1: T, x2: T): List[T] = new Const[T](x1, new Const[T](x2, new Nil[T]))
  def apply[T](x: T): List[T] = new Const[T](x, new Nil[T])
  def apply[T](): List[T] = new Nil[T]
}

object main {
  def main(args: Array[String]): Unit = {
    println(List(1,2))
    println(List(1))
    println(List())

    /* This marks a compile error on line 2, because it is trying to cast an array to another class that is not a
    a subtype
    val a: Array[NonEmpty] = Array(new NonEmpty(1, Empty, Empty))
    val b: Array[IntSet] = a
    b(0) = Empty
    val s: NonEmpty = a(0)*/
  }
}
