package week3

trait ConsTrait[T] {
  def isEmpty: Boolean
  def head: T
  def tail: T
  def nth(n: Int): T
}

class Cons[T](val head: T, val tail: ConsTrait[T]) extends ConsTrait[T]{

  def isEmpty: Boolean = false
  def nth(n: Int): T =  if (n == 0) this.head else this.tail.nth(n-1)
}

class Nil[T] extends ConsTrait[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  def nth(n: Int): Nothing = throw new NoSuchElementException("ConsTrait")
}

//def singleton[T](elem: [T]): ConList[T] = new Cons[T](elem, new Nil[T])

//singleton(27)

val lst = new Cons[Int](1,
          new Cons[Int](2,
          new Cons[Int](3,
          new Nil[Int])))

class ConsList[T](varargs: T*) {

  def nth[T](list: ConsTrait[T], n: Int) = {

  }

}