package exercises.week02

class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator must be nonzero")

  def this(x: Int) = this(x, 1)
  /**
   * Greatest common denominator
   **/
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def numer = x

  // Equivalent definition, as it is a val, gcd is called only once
  val denom = y

  def less(that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this.less(that)) that else this

  def add(that: Rational) = new Rational(
    numer * that.denom + that.numer * denom ,
    denom * that.denom
  )

  def neg: Rational = new Rational(-numer, denom)

  // println("As the constructor executes the whole block, this should print each time a Rational is created")

  def sub(that: Rational) = add(that.neg)

  override def toString: String = {
    val g = gcd(numer, denom)
    numer / g + "/" + denom / g
  }
}
