package automata.dag

import java.io._
import automata.tree.LearnTreeAutomata
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scala.collection.mutable.ListBuffer

object LearnDeterministicDag {
  //TODO: there has got tp be a better way to do this
  def reverseGraph[A](g: Graph[A, DiEdge]): Graph[A, DiEdge] = {
    val es = g.edges.map { case g.EdgeT(in, out) => (out.value ~> in.value) }
    Graph.from(g.nodes, es)
  }

  def writeGrammar(dagdfa: DagDfaFast[_]) = {
    val fos = new FileOutputStream("graphGrammar.obj")
    val oos = new ObjectOutputStream(fos)
    oos.writeObject(dagdfa)
    oos.close
  }

  def printStatistics[LABEL, A](g: Graph[A, DiEdge], time_cost: ListBuffer[(Double, Double)], dagdfa: DagDfaFast[LABEL])(describe: A => LABEL,describe_original: A => LABEL) = {
    val writer = new PrintWriter(new File("time_cost.csv"))
    for ((time, cost) <- time_cost) {
      writer.write(time.toString + ", " + "%.3f".format(cost).toString + "\n")
    }

    //    just compute it again
    val base = prefixSuffixDfa(g)(describe_original) //already minimized

    writer.close
    println
    println("=============Statistics==============")
    println
    println(" graph Vertices: " + g.nodes.size)
    println(" grapg node labels: " + g.nodes.map(n => describe_original(n)).size)
    println(" grapg Edges: " + g.edges.size)
    println

    println("Initial total input rules: " + base.inputTree.transitions.size)
    println("Initial total output rules: " + base.outputTree.transitions.size)
    println("Initial estimated language Cost: " + base.languageDescriptionCost)
    println("Initial estimated Cost given language: " + base.graphDescriptionCostGivenLanguage(g)(describe))
    println("Initial estimated MDL Cost Total: " + base.mdl(g)(describe_original))
    println("Initial destinguishable node types: " + base.okPairs.size)
    println("Initial Size in bytes: " + " -- ")
    println

    println("Final total input rules: " + dagdfa.inputTree.transitions.size)
    println("Final total output rules: " + dagdfa.outputTree.transitions.size)
    println("Final estimated language Cost: " + dagdfa.languageDescriptionCost)
    println("Final estimated Cost given language: " + dagdfa.graphDescriptionCostGivenLanguage(g)(describe))
    println("Final estimated MDL Cost Total: " + dagdfa.mdl(g)(describe) + " bits")
    println("Final destinguishable node types: " + dagdfa.okPairs.size)
    println("Final Size in bytes: " + " -- ")
    println(" Nodes merged: " + (base.okPairs.size - dagdfa.okPairs.size))
    println
    
    // I need parsing time invoked for other graphs. Maybe from outside. So I think it
    // it should be better to wrap function parse with time
    // val startTime = System.currentTimeMillis().toDouble / 1000.0
    // dagdfa.parse(g)(describe) // you may want to instead parse it on other graphs, but it will always parse fast and linear 
    // val endime = System.currentTimeMillis().toDouble / 1000.0
    println("Time to parse: " )
    println("Time spent in induction: " + " -- ")
    println("=============Statistics==============")
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

  private def meregeAll[A](a1: A, a2: A) = true
  //TODO: this is an ehuastive thing, make it stop early for the greedy
  def greedyLearn[LABEL, A](
    g: Graph[A, DiEdge], time: Double = Double.PositiveInfinity)( //, mergeHint:((A,A)=>Boolean) = meregeAll _ )(
      describe: A => LABEL, describe_original: A => LABEL): DagDfaFast[LABEL] = {
    var time_cost = new ListBuffer[(Double, Double)]()
    require(g.isDirected)
    require(g.isAcyclic)

    //TODO: some fancy scala way to do this?
    val startTime = System.currentTimeMillis().toDouble / 1000.0
    val endTime = startTime + time
    var timeLapsed = System.currentTimeMillis().toDouble

    println("...")
    val base = prefixSuffixDfa(g)(describe) //already minimized
    println(base)
    println("Done with Prefix Suffix DFA")

    var knownCosts = Map[DagDfaFast[LABEL], Double](base -> base.mdl(g)(describe))
    var activeParents = Set[DagDfaFast[LABEL]](base) //TODO: should be a priority queue
    var lowestSeenCost = knownCosts.values.last

    time_cost += (((System.currentTimeMillis().toDouble - timeLapsed) / 1000.0, lowestSeenCost))
    var timer = System.currentTimeMillis().toDouble
    println("Starting While Loop")
    while (!activeParents.isEmpty &&
      (System.currentTimeMillis().toDouble / 1000.0) < endTime) {
      val cheapest = activeParents.minBy(knownCosts)
      
      // Collect sample every xth second to see how mdl relate with time
      if ((System.currentTimeMillis().toDouble - timer) > 10000) {
        time_cost += (((System.currentTimeMillis().toDouble - timeLapsed) / 1000.0 -> lowestSeenCost))
        timer = System.currentTimeMillis().toDouble
      }

      if (knownCosts(cheapest) < lowestSeenCost) {
        time_cost += (((System.currentTimeMillis().toDouble - timeLapsed) / 1000.0 -> knownCosts(cheapest)))
        println(s"there are ${cheapest.okPairs.size} nodes to merge")
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
        val newDfa = cheapest.merge(a, b)
        if (!knownCosts.contains(newDfa)) {
          knownCosts += (newDfa -> newDfa.mdl(g)(describe))
          newParents += newDfa
        }
      }
      activeParents = (activeParents ++ newParents) - cheapest
    }

    printStatistics(g, time_cost, knownCosts.minBy(_._2)._1)(describe,describe_original)
    knownCosts.minBy(_._2)._1
  }
}
