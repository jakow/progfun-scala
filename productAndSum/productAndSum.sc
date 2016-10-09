def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(acc: Int, curr : Int): Int =
    if (curr > b) acc else loop(acc + f(curr), curr+1)
  if (a > b) 0 else loop(0, a)
}

sum((x:Int ) => x*x)(1,10)


def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)

product(x => x)(1,5)

def factorial(n: Int) = product(x => x)(1, n)
factorial(5)

def reduce(op: (Int,Int) => Int, f: Int => Int,identity: Int)(a:Int, b:Int): Int = {
  if (a > b) identity
  else op(f(a), reduce(op, f, identity)(a+1,b));
}

reduce((a: Int,b:Int) => (a*b),x=>x, 1)(1,10)