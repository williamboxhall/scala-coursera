val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x + y * z
x - y - z
y + y

class Rational(val numer: Int, val denom: Int) {
  require(denom != 0, "denominator must be nonzero")

  def this(numer: Int) = this(numer, 1)

  def +(that: Rational) =
    new Rational(this.numer * that.denom + this.denom * that.numer,
      this.denom * that.denom)

  def unary_- = new Rational(-numer, denom)

  def -(that: Rational) = this + -that

  def *(that: Rational) =
    new Rational(this.numer * that.numer,
      this.denom * that.denom)

  def /(that: Rational) =
    new Rational(this.numer * that.denom,
      this.denom * that.numer)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  override def toString = {
    val g = gcd(numer, denom)
    s"${numer / g}/${denom / g}"
  }
}