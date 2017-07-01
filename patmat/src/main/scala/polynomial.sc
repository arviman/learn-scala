package week6

object polynomials {
  class Poly(val terms: Map[Int, Double]){
    def + (other: Poly) = new Poly(terms++other.terms)

    override def toString =
      (for ((exp,coeff) <- terms.toList.sorted.reverse)
        yield coeff+"^"+exp) mkString " + "
  }

  var p1 = new Poly(Map(1->1, 2->3))

  p1.toString

}