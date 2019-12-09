package exercises.week04

trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }
  def isSum: Boolean = false
}

case class Number(n: Int) extends Expr {
}

case class Sum(e1: Expr, e2: Expr) extends Expr {
  override def isSum: Boolean = true
}

case class Variable(name: String) extends Expr {
}

case class Prod(e1: Expr, e2: Expr) extends Expr {
}

object ExprMain {

  def show(e: Expr): String = e match {
    case Number(n) => n.toString
    case Variable(x) => x
    case Prod(l, r) => if (l.isSum) "(" + show(l) + ") * " + show(r) else show(l) + " * " + show(r)
    case Sum(e1, e2) => show(e1) + " + " + show(e2)
  }

  def main(args: Array[String]): Unit = {
    val expr = Sum(Number(1), Sum(Number(2), Number(4)))
    println("eval: " + expr.eval)
    println("show: " + show(expr))

    val e1 = Sum(Prod(Number(2), Variable("x")), Variable("y"))
    val e2 = Prod(Sum(Number(2), Variable("x")), Variable("y"))
    println(show(e1))
    println(show(e2))
  }
}

