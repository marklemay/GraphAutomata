package automata.dag

import scala.collection.immutable.MultiSet._
import scala.collection.immutable.Bag //suprisingly important to be explicit about this
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import automata.tree.TreeDfa
import scalax.collection.edge.Implicits._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef
import automata.tree.LearnTreeAutomata

object LearnDeterministicDag {

  //TODO: there has got tp be a better way to do this
  def reverseGraph[A](g: Graph[A, DiEdge]): Graph[A, DiEdge] = {
    val es = g.edges.map { case g.EdgeT(in, out) => (out.value ~> in.value) }
    Graph.from(g.nodes, es)
  }

  //TODO: there has got tp be a better way to do this
  def describeg[A, LABEL](g: Graph[A, DiEdge])(describe: A => LABEL)(n: g.NodeT): LABEL = {
    describe(n.value)
  }

  def prefixSuffixDfa[LABEL, A](g: Graph[A, DiEdge])(describe: A => LABEL): DagDfa[LABEL, _, _] = {

    val rg = reverseGraph(g)

    val incoming = LearnTreeAutomata.prefixDFA(g)(describeg(g)(describe))
    val outgoing = LearnTreeAutomata.prefixDFA(rg)(describeg(rg)(describe))

    val inMap = incoming.parse(g)(describeg(g)(describe)).get.map(p => p._1.value -> p._2)
    val outMap = outgoing.parse(rg)(describeg(rg)(describe)).get.map(p => p._1.value -> p._2)

    assert(inMap.keySet == outMap.keySet)

    val fullMap = inMap.keySet.map(k => k -> (inMap(k), outMap(k))).toMap

    DagDfa(incoming, outgoing, fullMap.values.toSet)
  }

  def greedyLearn[LABEL,A](
    g: Graph[A, DiEdge])(
      describe: A => LABEL): DagDfa[LABEL, Int,Int] = {
    require(g.isDirected)
    require(g.isAcyclic)

    val base = prefixSuffixDfa(g)(describe).withIdIndex(g)(describe) //already minimized

    var knownCosts = Map[DagDfa[LABEL, Int, Int], Double](base -> base.mdl(g)(describe))

    var activeParents = Set[DagDfa[LABEL, Int, Int]](base)

    var lowestSeenCost = knownCosts.values.last

    while (!activeParents.isEmpty) {
      val cheapest = activeParents.minBy(knownCosts)

      if (knownCosts(cheapest) < lowestSeenCost) {
        println
        println("cost " + knownCosts(cheapest))
//        println(cheapest.transitions.mkString("\n"))
//        println(cheapest.roots)
        println
        lowestSeenCost = knownCosts(cheapest)
      } else {
        print(".")
      }


      var newParents = Set[DagDfa[LABEL, Int, Int]]()

      for (
        pairs <- cheapest.okPairs.subsets(2);
        List(a, b) = pairs.toList
      ) {

        
        val newDfa = cheapest.merge(a, b)(g)(describe).withIdIndex(g)(describe)

        if (!knownCosts.contains(newDfa)) {
          knownCosts += (newDfa -> newDfa.mdl(g)(describe))
          newParents += newDfa
        }

      }

      activeParents = (activeParents ++ newParents) - cheapest
    }

    knownCosts.minBy(_._2)._1
  }
}