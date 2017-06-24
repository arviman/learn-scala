package funsets

import java.util.NoSuchElementException

/**
  * Created by Arvind on 6/23/2017.
  */
trait List[T] {

  def isEmpty:Boolean
  def head: T
  def tail : List[T]


}

class Cons[T](val head:T, val tail:List[T]) extends List[T] {
  def isEmpty = true
}


class Nil[T] extends List[T] {
  def isEmpty = true
  def head:Nothing = throw new NoSuchElementException("Nil.head")
  def tail:Nothing = throw new NoSuchElementException("Nil.tail")
}

object nth{
  def nth[T](pos:Int, list:List[T]):T={
    if(pos==0) list.head
    else nth(pos-1, list.tail)
  }
}
