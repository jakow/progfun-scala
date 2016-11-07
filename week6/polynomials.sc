object polynomials {

  class Poly(val terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0
    def adjust(term: (Int,Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
    }
    def +(other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))

    private def addTerm (terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exp,coeff) = term
      terms + (exp -> (terms(exp) + coeff))
    }
    override def toString: String = {
      val sorted = this.terms.toList.sortWith(_._1 > _._1)
      def foldPoly(curr: String, value: (Int, Double)): String = {
        val term = "" + math.abs(value._2) + "x^" + value._1
      if (curr.isEmpty) {
        if (value._2 < 0) "- " + term
        else "" + term
      }
      else {
        if (value._2 < 0) curr + " - " + term
        else curr + " + " + term
      }
      }

      (sorted foldLeft "") (foldPoly)
    }
  }
    val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> -6.2)
    val p2 = new Poly(0 -> -3.0, 3 -> 7.0)
    p1 + p2

  }

