package automata.tree

import scala.collection.immutable.MultiSet._
import scala.collection.immutable.Bag
import scalax.collection.Graph
import scalax.collection.edge.Implicits._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge.DiEdge
import automata.tree.TreeAutomata.Transition

object Main {
  def main(args: Array[String]): Unit = {

        val m = scala.collection.immutable.DisjointSets.disjointEquivelence(Set(
      Set(1, 2), Set(2, 3), Set(1, 3),
      Set(4, 5)))


    println(m)
    
    
//    val m = scala.collection.immutable.DisjointSets
//    .mapToRepresentitive(Set(
//      Set(1, 2), Set(2, 3), Set(1, 3),
//      Set(4, 5))).withDefault(identity)
//
//    println(Set(1,2,3,4,5).groupBy(m).mkString("\n"))

    //    println(Set(1).subsets(2))
    //
    //    println("Hello, world!")
    //
    //    //TODO: turn some of the below into tests!
    //    val dfa = TreeDfa[Char, String](
    //      Set(
    //        Transition(Bag(), 'a', "leaf"),
    //        Transition(Bag("leaf"), 'a', "m"),
    //        Transition(Bag("m"), 'a', "m2"),
    //        Transition(Bag("m2"), 'a', "m2"),
    //
    //        Transition(Bag("m"), 'c', "c"),
    //        Transition(Bag("m2"), 'c', "c")),
    //      Set("m", "m2", "c"))
    //
    //    println(dfa.minimize.transitions.mkString("\n"))
    //
    //    //    val dfa = TreeDfa[Char, String](
    //    //      Set(
    //    //        Transition(Bag(), 'a', "leaf"),
    //    //        Transition(Bag("leaf", "leaf"), 'a', "leaf2"),
    //    //        Transition(Bag("leaf2", "leaf2"), 'a', "leaf2")),
    //    //      Set("leaf", "leaf2"))
    //    //
    //    //    println(dfa.minimize.transitions.mkString("\n"))
    //    //
    //    //    //
    //    //    //    val nfa = TreeNfa[Boolean, String](
    //    //    //      Set(
    //    //    //        Transition(Bag(), true, "true leaf"),
    //    //    //        Transition(Bag(), false, "false leaf"),
    //    //    //        Transition(Bag("true leaf"), true, "secret true middle"),
    //    //    //        Transition(Bag("secret true middle", "secret true middle"), true, "secret true middle"),
    //    //    //        Transition(Bag("false leaf"), true, "secret false middle"),
    //    //    //        Transition(Bag("secret false middle", "secret false middle"), true, "secret false middle"),
    //    //    //
    //    //    //        Transition(Bag(), true, "tl"),
    //    //    //        Transition(Bag("tl"), true, "tm")         ,
    //    //    //                Transition(Bag("tm", "tm"), true, "tm")
    //    //    //                //,
    //    //    //        //        Transition(Bag("false leaf"), true, "secret false middle"),
    //    //    //        //        Transition(Bag("secret false middle", "secret false middle"), true, "secret false middle")
    //    //    //        ),
    //    //    //      Set("secret true middle", "secret false middle", "tm"))
    //    //    //
    //    //    //      val dfa = nfa.toTreeDfa
    //    //    //      
    //    //    //      println(dfa.transitions.mkString("\n"))
    //    //    //    
    //    //    ////    dfa.
    //    //    //      
    //    //    //    //    val g = Graph(1 ~> 2, 2 ~> 3, 3 ~> 4, 4 ~> 5,
    //    //    //    //      10 ~> 11, 11 ~> 5,
    //    //    //    //      21 ~> 22, 22 ~> 23, 23 ~> 24, 24 ~> 5)
    //    //    //    //
    //    //    //    //    //    def describe(n: g.NodeT) = true
    //    //    //    //    //
    //    //    //    //    //    val dfa = TreeDfa(Set(Transition(Bag(0), true, 0), Transition(Bag[Int](), true, 0)), Set(0))
    //    //    //    //
    //    //    //    //    def describe(n: g.NodeT) = n.value % 2 == 0
    //    //    //    //
    //    //    //    //    val dfa = TreeAutomata.prefixDFA(g)(describe)
    //    //    //    //
    //    //    //    //    println
    //    //    //    //    println(dfa)
    //    //    //    //    println
    //    //    //    //    println(dfa.roots)
    //    //    //    //    println
    //    //    //    //    println(dfa.labels)
    //    //    //    //    println
    //    //    //    //    println(dfa.ids)
    //    //    //    //    println
    //    //    //    //    println(dfa.labels)
    //    //    //    //    println
    //    //    //    //    println(dfa.parse(g)(describe))
    //    //    //    //
    //    //    //    //    //    val b = Bag[String]()
    //    //    //    //    //    
    //    //    //    //    //    println(Bag("sss","sss","sss","aa") == Bag("sss","sss","sss","aa"))
    //    //    //    //    //    
    //    //    //    //    //    
    //    //    //    //    //    println(Bag("sss","sas","sss","sa").map(_(0)))
    //    //    //    //    //    
    //    //    //    //    //    println
    //    //    //    //    //    
    //    //    //    //    //    for(s <- Bag("sss","sss","sss","aa")){
    //    //    //    //    //       println(s)
    //    //    //    //    //    }
  }
}