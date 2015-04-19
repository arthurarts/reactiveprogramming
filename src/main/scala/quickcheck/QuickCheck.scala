package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = for {
    i <- Gen.choose(1, 100)
    h <- oneOf(const(empty), genHeap)
    n <- insert(i, h)
  } yield n

  // Implementation of the hints
  property(" If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.") = forAll { (i1:Int, i2:Int ) =>
    val m = if (i1 < i2) i1 else i2
    val h = insert(i2, insert(i1, empty))
    findMin(h) == m;
  }

  property("If you insert an element into an empty heap then delete the minimum the resulting heap should be empty.") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  def findAndDeleteMin(heap: H, as: List[Int]): List[Int] = {
    if (isEmpty(heap)) as
    else findMin(heap) :: findAndDeleteMin(deleteMin(heap), as)
  }

  // Hint: recursion and helper functions are your friends.
  property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.") = forAll { h: H =>
    val xs = findAndDeleteMin(h, Nil)
    xs == xs.sorted
  }

  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other.") = forAll { (h1:H, h2:H)  =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val minMeldedHeap = findMin(meld(h1, h2))
    minMeldedHeap == min1 || minMeldedHeap == min2
  }

  property("removing and adding a value will keep the minimum unchanged") = forAll { h: H =>
    val m = findMin(h)
    findMin(insert(m, deleteMin(h))) == m
  }

  property("injecting a value smaller than minimum, will make it the new minimum") = forAll {h: H =>
    val currentMin = findMin(h)
    findMin(insert(currentMin, h)) == currentMin
  }

  property("if two heaps h1 and h2 are melded together and the min is moved from h1 to h2 and the result is melded, the order of the ranks should still be the same") = forAll { (h1: H, h2: H) =>
    val minH1 = findMin(h1)
    val meldedHeap1 = meld(h1, h2)
    val meldedHeap2 = meld(deleteMin(h1), insert(minH1, h2))
    val xs1 = findAndDeleteMin(meldedHeap1, Nil)
    val xs2 = findAndDeleteMin(meldedHeap2, Nil)
    xs1 == xs2
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
