abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat

  def +(that: Nat): Nat

  def -(that: Nat): Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new IllegalStateException("Negative numbers not allowed")

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if (that.isZero) this else throw new IllegalStateException("Negative numbers not allowed")
}

class Succ(pred: Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = pred

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat = this.predecessor + that.successor

  override def -(that: Nat): Nat = if (that.isZero) this else this.predecessor - that.predecessor
}

val one = Zero.successor
val two = one.successor
two.predecessor.predecessor // 0
//two.predecessor.predecessor.predecessor // exception
val four = one + one + two // 4
four.predecessor.predecessor.predecessor.predecessor // 0
//four.predecessor.predecessor.predecessor.predecessor.predecessor // exception
val three = four - one
three - two
three - three
//three - four // exception