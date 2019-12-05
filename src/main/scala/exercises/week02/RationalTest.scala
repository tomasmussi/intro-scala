package exercises.week02

object RationalTest {

  def main(args: Array[String]): Unit = {
    val x = new Rational(2, 3)
    val y = new Rational(1, 3)
    println(x.add(y))
    println(x.neg)
    println(x.sub(y))
  }
}
