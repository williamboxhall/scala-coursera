val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x.add(y).mul(z)
x.sub(y).sub(z)

class Rational(val numer: Int, val denom: Int) {
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

  override def toString = s"$numer/$denom"
}