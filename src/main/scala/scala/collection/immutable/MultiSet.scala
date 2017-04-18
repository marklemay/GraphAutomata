package scala.collection.immutable

import scala.collection.generic.CanBuildFrom
import scala.collection.AbstractIterator
import scala.util.hashing.Hashing
import scala.collection.immutable.LimitBags.LimitingConfiguration

object MultiSet {
  //Things to make Bags useful
  //  implicit def defualtConfig[A] = Bag.configuration.compact[A]

  //TODO: this is a bad place to put this configuration
  implicit def bagMax[A] = LimitBags.LimitBags[A](12)

  implicit def defualtConfig[A] = {

    //    println("HI!!!!")

    LimitBags.compactLimited[A] //Bag.configuration.compact[A]
  }

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

  implicit def setToSetExtention[A](a: Set[A]) = SetExtention(a)

  case class ListExtention[A](val l: List[A]) {
    def toBag: Bag[A] = {
      val counts = l.groupBy(x => x).mapValues(_.size).toSeq
      Bag.from(counts: _*)
    }

  }

  implicit def listToListExtention[A](a: List[A]) = ListExtention(a)

}

object LimitBags {

  //I think these configurtions are weak sauce
  sealed trait LimitingConfiguration[A] {
    val limitNum: Int

    def limit(n: Int) = if (n < limitNum) { n } else { limitNum }
  }

  case class LimitBags[A](override val limitNum: Int) extends LimitingConfiguration[A]

  class MultiplicityBagBucketLimited[A](override val sentinel: A, val mult: Int)(implicit lim: LimitingConfiguration[A])
      extends MultiplicityBagBucket[A](sentinel, lim.limit(mult)) {
    require(lim.limitNum >= this.multiplicity)

    override def added(elem: A, count: Int) = {
      if (count > 0)
        new MultiplicityBagBucketLimited(sentinel, multiplicity + count)
      else
        this
    }

    override def addedBucket(bucket: collection.BagBucket[A]) = {
      new MultiplicityBagBucketLimited[A](sentinel, this.multiplicity + bucket.multiplicity(sentinel))
    }

    override def -(elem: A): MultiplicityBagBucket[A] = {
      new MultiplicityBagBucketLimited(sentinel, Math.max(0, multiplicity - 1))
    }

    override def removed(elem: A, count: Int): BagBucket = {
      new MultiplicityBagBucketLimited(sentinel, Math.max(0, multiplicity - count))
    }

    override def intersect(that: collection.BagBucket[A]): BagBucket =
      new MultiplicityBagBucketLimited(sentinel, Math.min(this.multiplicity, that.multiplicity(sentinel)))

    override def diff(that: collection.BagBucket[A]): BagBucket =
      new MultiplicityBagBucketLimited(sentinel, Math.max(this.multiplicity - that.multiplicity(sentinel), 0))

    override def distinct: BagOfMultiplicitiesBagBucket[A]#BagBucket = {
      if (multiplicity <= 1) this
      else new MultiplicityBagBucketLimited(sentinel, 1)
    }
  }

  private class HashedMultiplicityBagLimitedConfiguration[A](val equivClass: Equiv[A] with Hashing[A])(implicit lim: LimitingConfiguration[A]) extends HashedBagConfiguration[A] {
    def empty(sentinel: A): MultiplicityBagBucket[A] = new MultiplicityBagBucketLimited(sentinel, 0)
  }

  def compactLimited[A](implicit lim: LimitingConfiguration[A]): HashedBagConfiguration[A] = new HashedMultiplicityBagLimitedConfiguration(collection.HashedBagConfiguration.defaultHashedEquiv[A])

  //  
  //  
  //  
  //  import scala.collection.immutable.HashedBagConfiguration
  //  
  //  def compact[A]: HashedBagConfiguration[A] = new HashedBagConfiguration.HashedMultiplicityBagConfiguration(collection.HashedBagConfiguration.defaultHashedEquiv[A])
  //
  //  
  //  
  //  
  //  
  //    private class HashedMultiplicityBagConfiguration[A](val equivClass: Equiv[A] with Hashing[A]) extends immutable.HashedBagConfiguration[A] {
  //    def empty(sentinel: A): immutable.MultiplicityBagBucket[A] = new immutable.MultiplicityBagBucket(sentinel, 0)
  //  }
  //
  //  

}
