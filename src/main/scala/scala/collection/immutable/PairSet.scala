package scala.collection.immutable

import scala.collection.immutable.Set.Set2

object PairSet {

  case class PairSetSetExtention[A](val s: Set[A]) {
    def toSet2: Set2[A] = {
      require(s.size == 2)
      val List(a, b) = s.toList
      new Set2(a, b)
    }
  }

  implicit def setToPairSetSetExtention[A](a: Set[A]) = PairSetSetExtention(a)
  
  
//  TODO: Set2 destructor?
}

//case class PairSet[A](a: A, b: A) extends scala.collection.immutable.Set[A] {
//  require(a != b)
//
//  override def equals(o: Any) = o match {
//    case that: PairSet[A] => that.a == a && that.b == b || that.a == b && that.b == a
//    case _ => false
//  }
//  override def hashCode = a.hashCode * b.hashCode // commutative and unique
//
//
//  // Members declared in scala.collection.GenSetLike
//  override def iterator: Iterator[A] = Set(a, b).iterator
//
//  // Members declared in scala.collection.SetLike
//  override def -(elem: A): scala.collection.immutable.Set[A] = ???
//  override def +(elem: A): scala.collection.immutable.Set[A] = ???
//  override def contains(elem: A): Boolean = a == elem || b == elem
//
//}
