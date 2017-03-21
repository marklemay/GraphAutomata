package automata.tree

import scala.collection.immutable.MultiSet._
import scala.collection.immutable.Bag
import scalax.collection.Graph
import scalax.collection.edge.Implicits._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge.DiEdge
import automata.tree.TreeAutomata.Transition
import scalax.collection.GraphPredef
import automata.dag.LearnDeterministicDag

object DagMain {

  def main(args: Array[String]): Unit = {

    val g = Graph(
      "start" ~> "m+", "m+" ~> "mend",
      "start" ~> "m-", "m-" ~> "mend")
    println(g)

    def describe(n: String) = n.head

    val detdag = LearnDeterministicDag.prefixSuffixDfa(g)(describe)
    println(detdag)

    println(detdag.parse(g)(describe))

    //    val g = Graph(
    //      "start" ~> "+middle", "+middle" ~> "end",
    //      "start" ~> "-middle", "-middle" ~> "end")
    //
    //    def describe(n: String) = n.head
    //
    //    val detdag = LearnDeterministicDag.prefixSuffixDfa(g)(describe)
    //    println(detdag)
    //
    //    println(detdag.parse(g)(describe))
  }
}