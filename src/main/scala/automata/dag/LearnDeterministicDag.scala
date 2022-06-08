package automata.dag

import java.io._
import automata.tree.LearnTreeAutomata
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.GraphHelper._
import scala.collection.mutable.ListBuffer
import automata.tree.TreeDfaFast
import math.Logs._

object LearnDeterministicDag {

  def prefixSuffixDfa[LABEL, A](g: Graph[A, DiEdge])(describe: A => LABEL): DagDfa[LABEL] = {
    val rg = reverseGraph(g)
    val incoming = LearnTreeAutomata.prefixDFA(g)(describeNode(g)(describe)).toFast
    val outgoing = LearnTreeAutomata.prefixDFA(rg)(describeNode(rg)(describe)).toFast
    val inMap = incoming.parse(g)(describeNode(g)(describe)).get.map(p => p._1.value -> p._2)
    val outMap = outgoing.parse(rg)(describeNode(rg)(describe)).get.map(p => p._1.value -> p._2)
    assert(inMap.keySet == outMap.keySet)
    val fullMap = inMap.keySet.map(k => k -> (inMap(k), outMap(k))).toMap
    DagDfa(incoming, outgoing, fullMap.values.toSet)
  }

  /** greedy search for the minimum description length cost, uses backtracking */
  // TODO: a stream implementation if this search would be nicer
  def greedyLearn[LABEL, A](
    g: Graph[A, DiEdge], time: Double = Double.PositiveInfinity)(describe: A => LABEL, describe_original: A => LABEL): DagDfa[LABEL] = {

    require(g.isDirected, "graph must be directed")
    require(g.isAcyclic, "graph must be acyclic")

    val startTime = System.currentTimeMillis().toDouble / 1000.0
    val endTime = startTime + time
    val original_base = prefixSuffixDfa(g)(describe_original)

    // this is the maximal DAG automata, ie as much information as possible is stored in the DAG automata and it has no recursion
    val base = prefixSuffixDfa(g)(describe)

    var knownCosts = Map[DagDfa[LABEL], Double](base -> base.mdl(g)(describe))
    var activeParents = Set[DagDfa[LABEL]](base) //TODO: should be a priority que

    while (!activeParents.isEmpty && (System.currentTimeMillis().toDouble / 1000.0) < endTime) {
      val cheapest = activeParents.minBy(knownCosts)
      
      
      // explore the best possible merges from the best unexplored DFA
      var newParents = Set[DagDfa[LABEL]]()
      for (
        pairs <- cheapest.okPairs.subsets(2);
        List(a, b) = pairs.toList
      ) {
        val newDfa = cheapest.merge(a, b)
        if (!knownCosts.contains(newDfa)) {
          knownCosts += (newDfa -> newDfa.mdl(g)(describe))
          newParents += newDfa
        }
      }
      activeParents = (activeParents ++ newParents) - cheapest
    }

    return knownCosts.minBy(_._2)._1
  }


}
