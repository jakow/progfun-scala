import scala.annotation.tailrec

object nqueens {
  def queens(n: Int): Set[List[Int]] = {
    // my version
    def isSafe(col: Int, q: List[Int]): Boolean = {
      val row = q.length
      // due to short circuiting, isSafeLoop _should_ be tail recursive
      @tailrec
      def isSafeLoop(l: List[Int]) : Boolean = l match {
        case Nil => true
        // has to be not on the same column and not in diagonal. Test recursively for all queens
        case y :: ys => !isDiagonal(l.head, l.length - 1, col, row) && col != y && isSafeLoop(ys)
      }
      isSafeLoop(q)
    }

    def isDiagonal(x1: Int, y1: Int, x2: Int, y2: Int): Boolean =
      math.abs(x1 - x2) == math.abs(y1 - y2)
  // reference version similar to that from the
    def isSafe2(col: Int, q: List[Int]): Boolean = {
      val row = q.length
      val queensWithIdx = q zip (row - 1 to 0 by -1)
      queensWithIdx forall {
        case (c, r) => c != col && (math.abs(c - col) != row - r)
      }
    }



    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else for {
        q <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, q)
      }
        yield col :: q
    }
    placeQueens(n)
  }

  def show(queens: List[Int]) = {
    val lines = for (col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
    "\n" + (lines mkString "\n")
  }
  queens(4)
}

