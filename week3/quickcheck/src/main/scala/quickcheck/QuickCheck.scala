package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      h <- genHeap
    } yield insert(x, h.asInstanceOf[H])
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("prop1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("prop2") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("prop3") = forAll { a: (Int, Int) =>
    val h1 = insert(a._1, empty)
    val h2 = insert(a._2, h1)
    findMin(h2) ==  Math.min(a._1, a._2)
  }

  property("prop3.1") = forAll { a: (Int, Int, Int) =>
    val h1 = insert(a._1, empty)
    val h2 = insert(a._2, h1)
    val h3 = insert(a._3, h2)
    findMin(h3) ==   Math.min(Math.min(a._1, a._2), a._3)
  }

  property("prop4") = forAll { a: Int =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2)
  }

  property("prop4.1") = forAll { a: (Int, Int) =>
    val h1 = insert(a._1, empty)
    val h2 = insert(a._2, h1)
    val h3 = deleteMin(h2)
    findMin(h3) ==  Math.max(a._1, a._2)
  }

  property("prop4.2") = forAll { a: (Int, Int, Int) =>
    val h1 = insert(a._1, empty)
    val h2 = insert(a._2, h1)
    val h3 = insert(a._3, h2)
    val h4 = deleteMin(h3)
    val h5 = deleteMin(h4)
    findMin(h5) ==   Math.max(Math.max(a._1, a._2), a._3)
  }

  def findMinHelper(h: H): Int = {
    if (isEmpty(h)) Integer.MAX_VALUE else findMin(h)
  }

  def recursion(h: H): Boolean = {
    if (isEmpty(h)) {
      true
    } else {
      (findMin(h) <= findMinHelper(deleteMin(h))) && recursion(deleteMin(h))
    }
  }

  property("prop5") = forAll { (h: H) =>
    recursion(h)
  }

  property("prop6") = forAll { h: (H, H) ⇒
    val h1 = h._1
    val h2 = h._2
    val h3 = meld(h1, h2)
    val m1 = if (isEmpty(h1)) Integer.MAX_VALUE else findMin(h1)
    val m2 = if (isEmpty(h2)) Integer.MAX_VALUE else findMin(h2)
    val m3 = if (isEmpty(h3)) Integer.MAX_VALUE else findMin(h3)
    m3 == Math.min(m1, m2)
  }

}
