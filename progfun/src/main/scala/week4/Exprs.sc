trait Expr
case class Number(x: Int) extends Expr
case class Sum(l: Expr, r: Expr) extends Expr
def show(e: Expr): String = e match {
  case Number(x) => x.toString
  case Sum(l, r) => show(l) + "+" + show(r)
}
show(Sum(Number(1), Number(2)))


