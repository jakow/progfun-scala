def factorial(x: Int): Int = {
  if (x < 0) throw new IllegalArgumentException("Cannot take factorial of a negative number")
  def loop(x: Int, acc: Int): Int =
    if (x <= 1) acc else loop(x-1, acc * x)
  loop(x, 1)
}

factorial(-1)