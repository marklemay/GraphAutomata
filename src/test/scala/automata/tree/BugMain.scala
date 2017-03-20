package automata.tree
import scala.collection.immutable.MultiSet._
import scala.collection.immutable.Bag
import scalax.collection.Graph
import scalax.collection.edge.Implicits._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge.DiEdge
import automata.tree.TreeAutomata.Transition

object BugMain {

  def main(args: Array[String]): Unit = {
    println("bugs")

    val dfa = TreeDfa[Boolean, Int](
      Set(
        Transition(Bag(), false, (0)),
        Transition(Bag((0), (0)), true, (1)),
        Transition(Bag((1), (1)), true, (1)),
        Transition(Bag((0), (1)), true, (2)),
        Transition(Bag((1), (2)), true, (1))),

      Set( 1, 2))

    println(dfa.minimize)

    //        val dfa = TreeDfa[Boolean, Int](
    //      Set(
    //        Transition(Bag(), false, (0)),
    //        Transition(Bag((0), (0)), true, (4)),
    //        Transition(Bag((4), (4)), true, (0)),
    //        Transition(Bag((4), (0)), true, (6)),
    //        
    //        
    //        Transition(Bag(), true, (3)),
    //        Transition(Bag((3), (3)), true, (5)),
    //        Transition(Bag((5), (3)), true, (1)),
    //        Transition(Bag((5), (5)), true, (7))),
    //      Set((4), (5), (7), (0), (6), (1)))
    //    
    //    
    ////    val dfa = TreeDfa[Boolean, Set[Int]](
    ////      Set(
    ////        Transition(Bag(), false, Set(0)),
    ////        Transition(Bag(Set(4), Set(4)), true, Set(0)),
    ////        Transition(Bag(Set(4), Set(0)), true, Set(6)),
    ////        Transition(Bag(Set(3), Set(3)), true, Set(5)),
    ////        Transition(Bag(), true, Set(3)),
    ////        Transition(Bag(Set(0), Set(0)), true, Set(4)),
    ////        Transition(Bag(Set(5), Set(3)), true, Set(1)),
    ////        Transition(Bag(Set(5), Set(5)), true, Set(7))),
    ////      Set(Set(4), Set(5), Set(7), Set(0), Set(6), Set(1)))
    //      
    //dfa.minimize

    //    val dfa = TreeDfa[Boolean, Set[Int]](
    //      Set(
    //        Transition(Bag(), false, Set(0)),
    //        Transition(Bag(Set(0), Set(0)), true, Set(4)),
    //        Transition(Bag(Set(4), Set(4)), true, Set(0)),
    //        Transition(Bag(Set(4), Set(0)), true, Set(6)),
    //
    //        Transition(Bag(), true, Set(3)),
    //        Transition(Bag(Set(3), Set(3)), true, Set(5)),
    //        Transition(Bag(Set(5), Set(3)), true, Set(1)),
    //        Transition(Bag(Set(5), Set(5)), true, Set(7))),
    //      Set(
    //        Set(0), Set(4), Set(6),
    //        Set(5), Set(1), Set(7)))
    //
    //    //but this works
    ////        val dfa = TreeDfa[Boolean, Int](
    ////          Set(
    ////            Transition(Bag(), false, 0),
    ////            Transition(Bag((4), (4)), true, (0)),
    ////            Transition(Bag((4), (0)), true, (6)),
    ////            Transition(Bag((3), (3)), true, (5)),
    ////            Transition(Bag(), true, (3)),
    ////            Transition(Bag((0), (0)), true, (4)),
    ////            Transition(Bag((5), (3)), true, (1)),
    ////            Transition(Bag((5), (5)), true, (7))),
    ////          Set((4), (5), (7), (0), (6), (1)))
    //
    //    dfa.minimize
    //
    //    //incorrectly got to:
    //    //    val dfa = TreeDfa[Boolean, Set[Int]](
    //    //      Set(
    //    //        Transition(Bag(), false, Set(0)),
    //    //        Transition(Bag(Set(3), Set(3)), true, Set(5)),
    //    //        Transition(Bag(), true, Set(3)), Transition(Bag(Set(5), Set(3)), true, Set(1)),
    //    //        Transition(Bag(Set(0), Set(0)), true, Set(0)),
    //    //        Transition(Bag(Set(5), Set(5)), true, Set(6)),
    //    //        Transition(Bag(Set(0), Set(0)), true, Set(6))),
    //    //      Set(Set(0), Set(5), Set(6), Set(1)))

  }
}