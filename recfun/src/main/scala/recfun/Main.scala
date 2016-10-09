package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    print(countChange(120, List(2,1)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {

      if (c == 0 ||r == 0 || c == r ) 1 else pascal(r-1, c-1) + pascal(r-1, c)
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def count( str:List[Char], leftParens: Int, rightParens: Int): Boolean = {
        if (rightParens > leftParens) false
        else if (str.isEmpty) rightParens == leftParens
        else if (str.head == '(') count(str.tail, leftParens+1, rightParens)
        else if (str.head == ')') count(str.tail, leftParens, rightParens+1)
        else count(str.tail, leftParens, rightParens)
      }
      count(chars,0,0)

    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
        def countRemaining(remaining: Int, change: List[Int]): Int = {
          if(change.isEmpty || remaining < 0)  0
          else if (remaining == 0) 1
          else countRemaining(remaining - change.head, change) + countRemaining(remaining, change.tail)
        }
      if (money == 0) 0
      else countRemaining(money, coins)
    }
  }
