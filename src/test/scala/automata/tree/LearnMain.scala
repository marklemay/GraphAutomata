package automata.tree

import scala.collection.immutable.MultiSet._
import scala.collection.immutable.Bag
import scalax.collection.Graph
import scalax.collection.edge.Implicits._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge.DiEdge
import automata.tree.TreeAutomata.Transition

object LearnMain {

  def main(args: Array[String]): Unit = {
    //some examples enumeration style
    val g = Graph(
      1 ~> 3, 2 ~> 3,

      -11 ~> 13, -12 ~> 13,

      21 ~> 23, 22 ~> 23, 24 ~> 25, 23 ~> 25,
      -31 ~> 33, -32 ~> 33, -34 ~> 35, 33 ~> 35,

      41 ~> 43, 42 ~> 43, 44 ~> 45, 46 ~> 45, 43 ~> 47, 45 ~> 47,
      -51 ~> 53, -52 ~> 53, -54 ~> 55, -56 ~> 55, 53 ~> 57, 55 ~> 57,

      61 ~> 63, 62 ~> 63, 64 ~> 65, 66 ~> 65, 63 ~> 67, 65 ~> 67, 68 ~> 69, 67 ~> 69,
      -71 ~> 73, -72 ~> 73, -74 ~> 75, -76 ~> 75, 73 ~> 77, 75 ~> 77, -78 ~> 79, 77 ~> 79,

      81 ~> 83, 82 ~> 83, 84 ~> 85, 86 ~> 85, 83 ~> 87, 85 ~> 87, 88 ~> 89, 87 ~> 89, 801 ~> 88, 802 ~> 88,
      -91 ~> 93, -92 ~> 93, -94 ~> 95, -96 ~> 95, 93 ~> 97, 95 ~> 97, 98 ~> 99, 97 ~> 99, -901 ~> 98, -902 ~> 98)

    def describe(n: g.NodeT): Boolean = n > 0

    val ideal = TreeDfa[Boolean, String](
      Set(
        Transition(Bag(), true, "+"),
        Transition(Bag("+", "+"), true, "m+"),
        Transition(Bag("m+", "+"), true, "m+"),
        Transition(Bag("m+", "m+"), true, "m+"),

        Transition(Bag(), false, "-"),
        Transition(Bag("-", "-"), true, "m-"),
        Transition(Bag("m-", "-"), true, "m-"),
        Transition(Bag("m-", "m-"), true, "m-")),
      Set("m-", "m+"))

    val sortof = TreeDfa[Boolean, String](
      Set(
        Transition(Bag(), true, "+"),
        Transition(Bag("+", "+"), true, "+"),

        Transition(Bag(), false, "-"),
        Transition(Bag("-", "-"), true, "m-"),
        Transition(Bag("m-", "-"), true, "m-"),
        Transition(Bag("m-", "m-"), true, "m-")),
      Set("m-", "+"))

    val minimal = TreeDfa[Boolean, String](
      Set(
        Transition(Bag(), true, "+"),
        Transition(Bag(), false, "+"),
        Transition(Bag("+", "+"), true, "+")),
      Set("+"))

    //      println(dfa.minimize)

    println("ideal " + ideal.mdl(g)(describe))
    println("sortof " + sortof.mdl(g)(describe))
    println("minimal" + minimal.mdl(g)(describe))

    val dfaFound = LearnTreeAutomata.greedyLearn(g)(describe)

    println("done")
    println(dfaFound)
  }
}