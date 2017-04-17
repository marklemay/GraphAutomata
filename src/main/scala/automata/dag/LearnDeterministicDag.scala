package automata.dag

import java.io._
import automata.tree.LearnTreeAutomata
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scala.collection.mutable.ListBuffer
import automata.tree.TreeDfaFast

object LearnDeterministicDag {
  //TODO: there has got to be a better way to do this
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

  def getEdges(dagdfa: DagDfaFast[_]): Int = {
    val my_set = dagdfa.inputTree.transitions
    var sum = 0
    for (set <- my_set) {
      sum += set.from.size
    }
    return sum
  }
  def getBeforeAfter(before: Long, after: Long, i_nodes: Int, f_nodes: Int, i_edges: Int, f_edges: Int): (Int, Int) = {
    // Return size in kilobyes
    val before_cont: Double = 32.8
    val after_cont: Double = 40.8
    val b_add_size = (before_cont * i_nodes) + (12 * i_edges)
    val a_add_size = (after_cont * f_nodes) + (12 * f_edges)
    val before_size = before + b_add_size / 1000.0
    val after_size = after + a_add_size / 1000.0
    return (before_size.toInt, after_size.toInt)
  }

  def printStatistics[LABEL, A](g: Graph[A, DiEdge], time_cost: ListBuffer[(Double, Double)], dagdfa: DagDfaFast[LABEL])(describe: A => LABEL, describe_original: A => LABEL) = {
    val writer = new PrintWriter(new File("time_cost.csv"))

    for ((time, cost) <- time_cost) {
      writer.write(time.toString + ", " + "%.3f".format(cost).toString + "\n")
    }
    writer.close
    val writer2 = new PrintWriter(new File("stats.csv"))
    val base = prefixSuffixDfa(g)(describe_original) //already minimized
    writer2.write("i-graph-vertices," + g.nodes.size + "\n")
    writer2.write("i-graph-labels," + g.nodes.map(n => describe_original(n)).size + "\n")
    writer2.write("i-graph-edges," + g.edges.size + "\n")
    writer2.write("i-graph-input-rules," + base.inputTree.transitions.size + "\n")
    writer2.write("i-graph-output-rules," + base.outputTree.transitions.size + "\n")
    writer2.write("i-lang-cost," + base.languageDescriptionCost + "\n")
    writer2.write("i-lang-cost-given," + base.graphDescriptionCostGivenLanguage(g)(describe) + "\n")
    writer2.write("i-mdl," + base.mdl(g)(describe_original) + "\n")
    writer2.write("i-dist-nodes," + base.okPairs.size + "\n")

    writer2.write("f-graph-vertices," + dagdfa.inputTree.transitions.size + "\n")
    writer2.write("f-graph-edges," + getEdges(dagdfa) + "\n")
    writer2.write("f-graph-input-rules," + dagdfa.inputTree.transitions.size + "\n")
    writer2.write("f-graph-output-rules," + dagdfa.outputTree.transitions.size + "\n")
    writer2.write("f-lang-cost," + dagdfa.languageDescriptionCost + "\n")
    writer2.write("f-lang-cost-given," + dagdfa.graphDescriptionCostGivenLanguage(g)(describe) + "\n")
    writer2.write("f-mdl," + dagdfa.mdl(g)(describe) + "\n")
    writer2.write("f-dist-nodes," + dagdfa.okPairs.size + "\n")
    writer2.write("f-nodes-merged," + (base.okPairs.size - dagdfa.okPairs.size) + "\n")
    val bsize = SizeEstimator.estimate(base)
    val asize = SizeEstimator.estimate(dagdfa)
    println("*********************************")
    println("*********************************")
    println(bsize)
    println(asize)
    println("*********************************")
    println("*********************************")
    val my_sizes = getBeforeAfter(bsize, asize, g.nodes.size, dagdfa.inputTree.transitions.size, g.edges.size, getEdges(dagdfa))
    writer2.write("i-size-bytes," + my_sizes._1 + "\n")
    writer2.write("f-size-bytes," + my_sizes._2 + "\n")
    writer2.close
    // writer2.write("Time to parse: " + "\n")
    // writer2.write("Time spent in induction: " + " -- "+ "\n")
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

  //TODO: this is an ehuastive thing, make it stop early for the greedy
  def greedyLearn[LABEL, A](
    g: Graph[A, DiEdge], time: Double = Double.PositiveInfinity)(
      describe: A => LABEL, describe_original: A => LABEL): DagDfaFast[LABEL] = {

    var time_cost = new ListBuffer[(Double, Double)]()
    require(g.isDirected)
    require(g.isAcyclic)

    //TODO: some fancy scala way to do this?
    val startTime = System.currentTimeMillis().toDouble / 1000.0
    val endTime = startTime + time
    var timeLapsed = System.currentTimeMillis().toDouble
    val original_base = prefixSuffixDfa(g)(describe_original) //already minimized
    time_cost += (((System.currentTimeMillis().toDouble - timeLapsed) / 1000.0, original_base.mdl(g)(describe_original)))
    
    val base = prefixSuffixDfa(g)(describe) //already minimized
    println("Done with Prefix Suffix DFA")
    var knownCosts = Map[DagDfaFast[LABEL], Double](base -> base.mdl(g)(describe))
    var activeParents = Set[DagDfaFast[LABEL]](base) //TODO: should be a priority queue
    var lowestSeenCost = knownCosts.values.last
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
    val dag_cust = knownCosts.minBy(_._2)._1

    return dag_cust
  }

  import scala.collection.immutable.MultiSet._
  import automata.tree.TreeAutomata._

  def addNeededTreeTransitions[A, LABEL](transitions: Set[Transition[LABEL, Int]], g: Graph[A, DiEdge])(describe: A => LABEL): Set[Transition[LABEL, Int]] = {

    var inTransitions = transitions

    val Right(ts) = g.topologicalSort

    var map = Map[g.NodeT, Int]()

    for (
      node <- ts;
      label = describe(node)
    ) {
      val parentStates = node.inNeighbors.toBag.map(map)

      inTransitions.find(t => t.from == parentStates && t.label == label) match {
        case Some(Transition(_, _, id)) => {
          map += node -> id
        }
        case None => {
          val newId = (inTransitions.map(_.to).max + 1)
          inTransitions += Transition(parentStates, label, newId)
          map += node -> newId
        }
      }

    }

    inTransitions
  }

  def maximallyExtendGrammar[A, LABEL](dfa: DagDfaFast[LABEL], g: Graph[A, DiEdge])(describe: A => LABEL): DagDfaFast[LABEL] = {

    val rg = reverseGraph(g)
    val incoming = TreeDfaFast(addNeededTreeTransitions(dfa.inputTree.transitions, g)(describe))
    assert(dfa.inputTree.transitions.subsetOf(incoming.transitions)) //.subsetOf(dfa.inputTree.transitions), incoming.transitions.diff(dfa.inputTree.transitions))

    val outgoing = TreeDfaFast(addNeededTreeTransitions(dfa.outputTree.transitions, rg)(describe))
    //    assert(outgoing.transitions.subsetOf(dfa.outputTree.transitions))

    val inMap = incoming.parse(g)(describeg(g)(describe)).get.map(p => p._1.value -> p._2)
    val outMap = outgoing.parse(rg)(describeg(rg)(describe)).get.map(p => p._1.value -> p._2)
    assert(inMap.keySet == outMap.keySet)
    val fullMap = inMap.keySet.map(k => k -> (inMap(k), outMap(k))).toMap

    DagDfaFast(incoming, outgoing, fullMap.values.toSet ++ dfa.okPairs)
  }

  //TODO: rename
  def hackyCost[A, LABEL](dfa: DagDfaFast[LABEL], g: Graph[A, DiEdge], describe: A => LABEL, oldNodeTypes: Set[(Int, Int)], bigNumber: Double = 20): Double = {

    //heavily wieght existing nodes
    val weightPerOldType = bigNumber / oldNodeTypes.size.toDouble

    //TODO: wouldbe less hacky with a bag implementation
    //TODO: so node modfications are weighted by past evidence
    //TODO: would require cleaning my other Bag hack

    var inertia = 0.0
    for ((inId, outId) <- oldNodeTypes) {
      inertia += weightPerOldType * log2(dfa.inputTree.idToTransitions(inId).size)
      inertia += weightPerOldType * log2(dfa.outputTree.idToTransitions(outId).size)

    }

    dfa.mdl(g)(describe) + inertia
  }

  //TODO: rename
  def findNextBestMerged[A, LABEL](dfa: DagDfaFast[LABEL], newDfa: DagDfaFast[LABEL], g: Graph[A, DiEdge])(describe: A => LABEL): DagDfaFast[LABEL] = {
    //TODO:  this stuff relies on some hidden assumptions ie that the indexes merge into the lower number

    val newNodeTypes = newDfa.okPairs -- dfa.okPairs

    var bestDfa = newDfa
    var bestCost = hackyCost(newDfa, g, describe, dfa.okPairs)

    for (
      newNodeType <- newNodeTypes;
      oldNodeType <- dfa.okPairs //;
    //if newNodeType._1 == oldNodeType._1 || newNodeType._2 == oldNodeType._2
    ) {

      val posibleMatch = newDfa.merge(newNodeType, oldNodeType)

      //disallow the compression of existing states
      if (dfa.okPairs.subsetOf(posibleMatch.okPairs)) {

        println(newNodeType)
        println(oldNodeType)
        println

        val cost = hackyCost(posibleMatch, g, describe, dfa.okPairs)

        println(cost)
        println(posibleMatch)

        if (cost < bestCost) {
          println("newbest")

          bestDfa = posibleMatch
          bestCost = cost

        }

        println
      }

    }

    bestDfa
  }

  /**
   * creates a minimally more general grammar such that g is paresable,
   *  it will greedily minimize until a local minimum is hit, making it a n^3 worst case
   */
  def augmentGrammar[A, LABEL](dfa: DagDfaFast[LABEL], g: Graph[A, DiEdge])(describe: A => LABEL): DagDfaFast[LABEL] = {
    //TODO:  this stuff relies on some hidden assumptions ie that the indexes merge into the lower number
    val newDfa = maximallyExtendGrammar(dfa, g)(describe)

    println(newDfa)

    val newNodeTypes = newDfa.okPairs -- dfa.okPairs

    var bestDfa = newDfa
    var bestCost = hackyCost(newDfa, g, describe, dfa.okPairs)

    //TODO: better
    while (true) {
      println
      println("merge")
      println

      val possible = findNextBestMerged(dfa, newDfa, g)(describe)

      val cost = hackyCost(possible, g, describe, dfa.okPairs)

      if (cost < bestCost) {

        bestDfa = possible
        bestCost = cost
      } else {
        return bestDfa
      }
    }

    bestDfa
  }
}
