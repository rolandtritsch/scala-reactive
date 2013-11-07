package quickcheck

import common._

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  property("min1") = forAll {a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll {(a: Int, b: Int) =>
    (a > b) ==> (findMin(insert(b, insert(a, empty))) == b)
  }

  property("min3") = forAll {(a: Int, b: Int) =>
    (a > b) ==> (findMin(deleteMin(insert(b, insert(a, empty)))) == a)
  }

  property("empty1") = forAll {(a: Int, b: Int) =>
    isEmpty(deleteMin(deleteMin(insert(b, insert(a, empty)))))
  }

  property("empty2") = forAll {h: H =>
    val l = heapToList(h, Nil)
    l.sorted == l
  }

  property("meld1") = forAll {h: H =>
    findMin(meld(h, empty)) == findMin(h)
  }

  property("meld2") = forAll {(h: H, a: Int) =>
    if(findMin(h) > a) {
      findMin(meld(h, insert(a, empty))) == a
    } else {
      findMin(meld(h, insert(a, empty))) == findMin(h)
    }
  }

  property("meld3") = forAll {(h1: H, h2: H) =>
    val l1 = heapToList(h1, Nil)
    val l2 = heapToList(h2, Nil)
    val l3 = heapToList(meld(h1, h2), Nil)
    (l1 ::: l2).sorted == l3
  }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[A]
    h <- oneOf(value(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  private def heapToList(h: H, l: List[A]): List[A] = {
    if(isEmpty(h)) l
    else heapToList(deleteMin(h), l :+ findMin(h))
  }
}
