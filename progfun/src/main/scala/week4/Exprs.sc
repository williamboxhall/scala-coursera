trait Expr
case class Number(x: Int) extends Expr
case class Sum(l: Expr, r: Expr) extends Expr
case class Prod(l: Expr, r: Expr) extends Expr
case class Var(x: String) extends Expr
def show(e: Expr, brace: Boolean = false): String = e match {
  case Number(x) => x.toString
  case Sum(l, r) => (if (brace) "(" else "") + show(l, true) + "+" + show(r, true) + (if (brace) ")" else "")
  case Prod(l, r) => (if (brace) "(" else "") + show(l, true) + "*" + show(r, true) + (if (brace) ")" else "")
  case Var(x) => x.toString
}
show(Sum(Number(1), Number(2)))
show(Prod(Number(1), Number(2)))
show(Sum(Prod(Number(2), Var("x")), Var("y")))
show(Prod(Sum(Number(2), Var("x")), Var("y")))

