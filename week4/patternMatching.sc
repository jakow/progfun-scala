//package week4
trait Expr
case class Number(val1: Int) extends Expr
case class Sum(val1: Expr, val2: Expr) extends Expr

def eval(e: Expr): Int = e match {
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
}

def show(e: Expr): String = e match {
  case Number(n) => n.toString
  case Sum(e1, e2) => show(e1) + " + " + show(e2)
}
val ex = Sum(Number(1), Number(2))
println(show(ex) + " = " + eval(ex))

//println("Hello world!")