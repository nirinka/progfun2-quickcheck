package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    m <- oneOf(const(empty), genHeap)
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min") = forAll {(n1: A, n2: A) =>
    val h = insert(n1, insert(n2, empty))
    findMin(h) == min(n1, n2)
  }

  property("delete") = forAll{(a: A) =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2)
  }

  property("mend") = forAll{(a1: A, a2: A, b1: A, b2: A) =>
    val h1 = insert(a1, insert(a2, empty))
    val h2 = insert(b1, insert(b2, empty))
    val h3 = meld(h1, h2)
    findMin(h3) == min(findMin(h1), findMin(h2))
  }

  property("gen2") = forAll{(h: H) =>
    def findAndDeleteMin(heap: H, elements: List[A]): List[A] = heap match {
      case empty => elements
      case _ => findAndDeleteMin(deleteMin(h), findMin(h) :: elements)
    }
    val result = findAndDeleteMin(h, List())
    result == result.sorted
  }

}
