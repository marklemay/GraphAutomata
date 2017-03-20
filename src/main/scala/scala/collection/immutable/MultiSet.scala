package scala.collection.immutable

import scala.collection.generic.CanBuildFrom
import scala.collection.AbstractIterator

object MultiSet {
  //Things to make Bags useful
  implicit def defualtConfig[A] = Bag.configuration.compact[A]

  case class SetExtention[A](val s: Set[A]) {
    def toBag: Bag[A] = {
      val counts = s.map((_, 1)).toSeq
      Bag.from(counts: _*)
    }

    //TODO: better as an iterator
    //TODO: better to Bags
    def sequences(n: Int): Set[List[A]] = {
      require(n >= 0)
      if (n == 0) {
        Set(List())
      } else {
        val subSeqs = sequences(n - 1)

        for (a <- s; subSeq <- subSeqs) yield (a :: subSeq)
      }
    }

    //TODO: better as an iterator
    //    def combinations(n: Int): Set[Bag[A]] = {
    //      require(n >= 0)
    //      
    //      s.subsets(n).map(a=>Bag(a))
    //      
    //      
    //      if (n == 0) {
    //        Set(Bag[A]())
    //      } else {
    //
    //        ???
    //      }
    //
    //    }
  }
  //  Bag.from(elemCounts)

  implicit def setToSetExtention[A](a: Set[A]) = SetExtention(a)

  case class ListExtention[A](val l: List[A]) {
    def toBag: Bag[A] = {
      val counts = l.groupBy(x => x).mapValues(_.size).toSeq
      Bag.from(counts: _*)
    }

  }
  //  Bag.from(elemCounts)

  implicit def listToListExtention[A](a: List[A]) = ListExtention(a)

  //    case class BagExtention[A](val s: Bag[A]) {
  //    
  //        def subsets(): Iterator[Bag[A]] = new AbstractIterator[Bag[A]] {
  //    private val elms = self.toIndexedSeq
  //    private var len = 0
  //    private var itr: Iterator[This] = Iterator.empty
  //
  //    def hasNext = len <= elms.size || itr.hasNext
  //    def next = {
  //      if (!itr.hasNext) {
  //        if (len > elms.size) Iterator.empty.next()
  //        else {
  //          itr = new SubsetsItr(elms, len)
  //          len += 1
  //        }
  //      }
  //
  //      itr.next()
  //    }
  //  }
  //    
  //
  //  implicit def bagToBagExtention[A](bag: Bag[A]) = BagExtention(bag)

  //  implicit def canBuildFrom[A]: CanBuildFrom[Bag[_], A, Bag[A]] = ??? ///setCanBuildFrom[A]

  //type testing

  val v = Bag(1, 1, 1, 2, 3, 4, 5, 5).map(_ % 2 == 0) //(scala.collection.immutable.MultiSet.canBuildFrom)

  //  case class IteratorExtention[A](val s: Set[A]) {
  //    def toBag: Bag[A] = {
  //      val counts = s.map((_, 1)).toSeq
  //      Bag.from(counts: _*)
  //    }
  //  }
  //  //  Bag.from(elemCounts)
  //
  //  implicit def setToSetExtention[A](a:Set[A]) = SetExtention(a)
}