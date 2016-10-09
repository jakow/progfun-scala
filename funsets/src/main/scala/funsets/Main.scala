package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  def set1 = (x:Int) => x < 5
  def set2 = (x:Int) => x > -4
  println(contains(intersect(set1, set2), 2))
  println(contains(union(set1, set2), 2))
  println(contains(diff(set1, set2), 2))
  def positiveNumbers = (x: Int) => x > 0
  def oddNumbers = map(positiveNumbers, x => 2*x-1)
  printSet(oddNumbers)

}
