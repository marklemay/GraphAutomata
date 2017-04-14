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

  def prefixSuffixDfa[LABEL, A](g: Graph[A, DiEdge])(describe: A => LABEL): DagDfaFast[LABEL] = {

    val rg = reverseGraph(g)

    val incoming = LearnTreeAutomata.prefixDFA(g)(describeg(g)(describe)).toFast
    val outgoing = LearnTreeAutomata.prefixDFA(rg)(describeg(rg)(describe)).toFast

    val inMap = incoming.parse(g)(describeg(g)(describe)).get.map(p => p._1.value -> p._2)
    val outMap = outgoing.parse(rg)(describeg(rg)(describe)).get.map(p => p._1.value -> p._2)

    assert(inMap.keySet == outMap.keySet)

    val fullMap = inMap.keySet.map(k => k -> (inMap(k), outMap(k))).toMap

    DagDfaFast(incoming, outgoing, fullMap.values.toSet)
  }

  private def meregeAll[A](a1:A,a2:A) = true
  //TODO: this is an ehuastive thing, make it stop early for the greedy
  def greedyLearn[LABEL, A](
    g: Graph[A, DiEdge], time: Double = Double.PositiveInfinity)( //, mergeHint:((A,A)=>Boolean) = meregeAll _ )(
      describe: A => LABEL): DagDfaFast[LABEL] = {
    require(g.isDirected)
    require(g.isAcyclic)

    //TODO: some fancy scala way to do this?
    val startTime = System.currentTimeMillis().toDouble / 1000.0
    val endTime = startTime + time

    println("...")

    val base = prefixSuffixDfa(g)(describe)  //already minimized
    
                println(base)
    println("Done with Prefix Suffix DFA")
    var knownCosts = Map[DagDfaFast[LABEL], Double](base -> base.mdl(g)(describe))

    var activeParents = Set[DagDfaFast[LABEL]](base) //TODO: should be a priority queue

    var lowestSeenCost = knownCosts.values.last
    println("Starting While Loop")
    while (!activeParents.isEmpty &&
      (System.currentTimeMillis().toDouble / 1000.0) < endTime) {
      val cheapest = activeParents.minBy(knownCosts)

      println(s"there are ${cheapest.okPairs.size} nodes to merge")

      if (knownCosts(cheapest) < lowestSeenCost) {
        println
        println("cost " + knownCosts(cheapest))
                println(cheapest)
        //        println(cheapest.roots)
        println
        lowestSeenCost = knownCosts(cheapest)
      } else {
        print(".")
      }

      var newParents = Set[DagDfaFast[LABEL]]()

      for (
        pairs <- cheapest.okPairs.subsets(2);
        List(a, b) = pairs.toList
      ) {

//        print("-")

        val newDfa = cheapest.merge(a, b)

//        print("!")
        if (!knownCosts.contains(newDfa)) {
          knownCosts += (newDfa -> newDfa.mdl(g)(describe))
          newParents += newDfa
        }

//        print("!")

      }

      activeParents = (activeParents ++ newParents) - cheapest
    }

    knownCosts.minBy(_._2)._1
  }
}
