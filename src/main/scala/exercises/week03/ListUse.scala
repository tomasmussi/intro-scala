package exercises.week03

object ListUse {

  def nth[T](n: Int, xs: List[T]): T = {
    if (n == 0) xs.head
    else if (xs.tail.isEmpty) throw new IndexOutOfBoundsException("Out of bounds")
    else nth(n - 1, xs.tail)
  }

  def main(args: Array[String]): Unit = {
    val l = new Const(1, new Const(2, new Const(3, new Nil)))
    println(nth[Int](0, l))
    println(nth[Int](1, l))
    println(nth[Int](2, l))
    println(nth[Int](-1, l))
    println(nth[Int](3, l))
  }
}
