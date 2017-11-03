package scala.collection.immutable

import scala.collection.generic.CanBuildFrom
import scala.collection.AbstractIterator
import scala.util.hashing.Hashing

/**
 * Things to make Bags easier to use
 */
object MultiSet {

  /** Handle configurations for the entire project */
  implicit def defualtConfig[A] = Bag.configuration.compact[A]

  
  /** Decorate Sets with toBag conversion */
  case class SetExtention[A](val s: Set[A]) {
    def toBag: Bag[A] = {
      val counts = s.map((_, 1)).toSeq
      Bag.from(counts: _*)
    }
  }

  /** Decorate Sets with toBag conversion */
  implicit def setToSetExtention[A](a: Set[A]) = SetExtention(a)

  /** Decorate Lists with toBag conversion */
  case class ListExtention[A](val l: List[A]) {
    def toBag: Bag[A] = {
      val counts = l.groupBy(x => x).mapValues(_.size).toSeq
      Bag.from(counts: _*)
    }
  }

  /** Decorate Lists with toBag conversion */
  implicit def listToListExtention[A](a: List[A]) = ListExtention(a)

}

