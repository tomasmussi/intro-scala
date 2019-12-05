package exercises.week02

class ImprovedRational(x: Int, y: Int) {
  require(y != 0, "Denominator must be nonzero")

  def this(x: Int) = this(x, 1)
  /**
   * Greatest common denominator
   **/
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  val numer = x

  val denom = y

  def <(that: ImprovedRational) = numer * that.denom < that.numer * denom

  def max(that: ImprovedRational) = if (this < that) that else this

  def +(that: ImprovedRational) = new ImprovedRational(
    numer * that.denom + that.numer * denom ,
    denom * that.denom
  )

  // def neg: ImprovedRational = new ImprovedRational(-numer, denom)
  def unary_- = new ImprovedRational(-numer, denom)

  // println("As the constructor executes the whole block, this should print each time a Rational is created")

  def -(that: ImprovedRational) = this + -that

  override def toString: String = {
    val g = gcd(numer, denom)
    numer / g + "/" + denom / g
  }
}
