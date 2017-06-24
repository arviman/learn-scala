package funsets


/**
  * 2. Purely Functional Sets.
  */
object FunSets {
  /**
    * We represent a set by its characteristic function, i.e.
    * its `contains` predicate.
    */
  type Set = Int => Boolean

  /**
    * Indicates whether a set contains a given element.
    */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
    * Returns the set of the one given element.
    */
  def singletonSet(id: Int): Set = {
    def newset(elem: Int): Boolean = id == elem
    newset
  }

  /**
    * Returns the union of the two given sets,
    * the sets of all elements that are in either `s` or `t`.
    */
  def union(s: Set, t: Set): Set = {
    def newset(elem: Int) = s(elem) || t(elem)
    newset
  }

  /**
    * Returns the intersection of the two given sets,
    * the set of all elements that are both in `s` and `t`.
    */
  def intersect(s: Set, t: Set): Set = {
    def newset(elem: Int) = s(elem) && t(elem)
    newset
  }

  /**
    * Returns the difference of the two given sets,
    * the set of all elements of `s` that are not in `t`.
    */
  def diff(s: Set, t: Set): Set = {
    def newset (elem: Int) = s(elem) && !t(elem)
    newset
  }

  /**
    * Returns the subset of `s` for which `p` holds.
    */
  def filter(s: Set, p: Int => Boolean): Set = {
    def newset(elem: Int) = s(elem) && p(elem)
    newset
  }


  /**
    * The bounds for `forall` and `exists` are +/- 1000.
    */
  val bound = 1000

  /**
    * Returns whether all bounded integers within `s` satisfy `p`.
    */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (contains(s, a) && !p(a) ) false
      else if (a > bound) true
      else iter(a+1)
    }
    iter(-bound)
  }

  /**
    * Returns whether there exists a bounded integer within `s`
    * that satisfies `p`.
    */
  def exists(s: Set, p: Int => Boolean): Boolean = {
    def notp (item:Int) = !p(item)
    !forall(s, notp )
  }
  /*

Forall
True T T T T T
False T F F F F
False F F F F F

Exists
True T T T T T
True T F F F F
False F F F F F


   */

  /**
    * Returns a set transformed by applying `f` to each element of `s`.
    */
  def map(s: Set, f: Int => Int): Set = {

    var added: Boolean = false
    var ns = s
    for(i <- -bound to bound)if(contains(s, i)){
      if(!added){
        ns = singletonSet(f(i))
        added = true
      }
      else{
        ns = union(ns, singletonSet(f(i)))
      }
    }
    ns
  }

  /* stack overflows and i don't know how to make it tail-rec
  def mapfunc(s: Set, f: Int => Int): Set = {

    def loop(s: Set, f:Int=>Int, start:Int):Set = {
      if(start == bound){
        if(contains(s, start)) {
          union(s, singletonSet(f(start)))
        }
        else s
      }

      val nextset = loop(s,f,start+1)
      if(contains(s, start)) {
        union(singletonSet(f(start)), nextset)
      }
      nextset
    }

    for(i <- -bound to bound)if(contains(s, i)){
      return loop(s,f,i)
    }
    s    //should never get here
  }*/

  /**
    * Displays the contents of a set
    */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
    * Prints the contents of a set on the console.
    */
  def printSet(s: Set) {
    println(toString(s))
  }
}
