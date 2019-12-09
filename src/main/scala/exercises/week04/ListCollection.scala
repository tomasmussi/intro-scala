package exercises.week04

object ListCollection {
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case scala.List() => scala.List(x)
    case y :: ys => if (x < y) x :: xs else y :: insert(x, ys)
  }

  def isort(xs: List[Int]): List[Int] = xs match {
    case scala.List() => scala.List()
    case y :: ys => insert(y, isort(ys))
  }

  def main(args: Array[String]): Unit = {
    val l = scala.List(4, 3, 2, 10, 8, 5)
    // l.foreach(println)
    isort(l).foreach(println)
  }

}