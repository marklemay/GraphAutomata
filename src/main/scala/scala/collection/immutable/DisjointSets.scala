package scala.collection.immutable

//perfect for all your equivelence class needs
object DisjointSets {

  //TODO: this whole thing is slower that it needs to be
  def disjointEquivelence[A](ss: Set[Set[A]]): Set[Set[A]] = {

    var newSets = Set[Set[A]]()

    for (s <- ss) {
      //      println(newSets)

      val oldSets = newSets.filter(!_.intersect(s).isEmpty)

      //      println(" "+oldSets)
      //      if (!oldSets.isEmpty) {
      val mewSet = (oldSets + s).reduce(_ union _)

      //      println(" n"+mewSet)
      newSets = (newSets -- oldSets) + mewSet
      //      } else {
      //
      //        newSets = newSets + s
      //      }

    }

    newSets
  }

  def mapToRepresentitive[A](ss: Set[Set[A]]): Map[A, A] = {
    val dijoint = disjointEquivelence(ss)

//    println(dijoint)
    
    val m = for (
      s <- dijoint;
      last = s.last;
      a <- s
    ) yield (a -> last)

    m.toMap
  }

  //TODO: use union find to make this super efficient, since it can amortize computation on demand
  //TODO: use ordering to keep it unique over different representations?
  //  case class DisjointSets[A](sets:Set[Set[A]]) extends Set[Set[A]]{
  //    
  //  }

}