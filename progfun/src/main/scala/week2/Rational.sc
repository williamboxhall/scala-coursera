val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x.add(y).mul(z)
x.sub(y).sub(z)
y.add(y)

class Rational(val numer: Int, val denom: Int) {
  require(denom != 0, "denominator must be nonzero")

  def this(numer: Int) = this(numer, 1)

  def add(that: Rational) =
    new Rational(this.numer * that.denom + this.denom * that.numer,
      this.denom * that.denom)

  def neg = new Rational(-numer, denom)

  def sub(that: Rational) =
    add(that.neg)

  def mul(that: Rational) =
    new Rational(this.numer * that.numer,
      this.denom * that.denom)

  def div(that: Rational) =
    new Rational(this.numer * that.denom,
      this.denom * that.numer)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  override def toString = {
    val g = gcd(numer, denom)
    s"${numer/g}/${denom/g}"
  }
}