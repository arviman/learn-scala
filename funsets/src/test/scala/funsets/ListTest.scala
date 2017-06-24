package funsets.test

//import funsets.FunSets._


import org.scalatest.FunSuite
import funsets.{Nil, Cons}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
/**
  * Created by Arvind on 6/23/2017.
  */
@RunWith(classOf[JUnitRunner])
class ListTest extends FunSuite {
  test("contains is implemented") {

    val list:Cons[Int]= new Cons(1, new Cons(2,new Nil[Int]))
    assert(true)
    assert(funsets.nth.nth[Int](1, list) == 2)
    assert(funsets.nth.nth[Int](0, list) == 1)

    }
    //assert(nth(1, list) == 2)

  }
}
