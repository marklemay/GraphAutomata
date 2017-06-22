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
import LearnDeterministicDag._
import automata.tree.TreeDfaFast

import automata.tree.TreeAutomata.log2

//TODO: not thrilled with this name
/**
 * defines a "dag grammar" such that  each node is completely characterized by it's visible inpt and its visable output
 *  this is achieved by an input and output tree automata *
 */
case class DagDfaFast[LABEL](
    //what can deduce about a node from its inputs
    inputTree: TreeDfaFast[LABEL],
    //what can deduce about a node from its outputs
    outputTree: TreeDfaFast[LABEL],

    //TODO: rename to nodeTypes?  it's good to think of these as the "type" of node, a label can belong to multiple types with additional conditions on input and output.
    okPairs: Set[(Int, Int)]) {

  def parse[A](g: Graph[A, DiEdge])(describe: A => LABEL): Option[Map[A, (Int, Int)]] = {

    val reverse = reverseGraph(g)
    val startTime = System.currentTimeMillis().toDouble
    (inputTree.parse(g)(describeg(g)(describe)), outputTree.parse(reverse)(describeg(reverse)(describe))) match {
      case (Some(inMap), Some(outMap)) => {
        val ins = inMap.map(p => p._1.value -> p._2)
        val outs = outMap.map(p => p._1.value -> p._2)

        val m = g.nodes.map(_.value).map(a => a -> (ins(a), outs(a))).toMap

        if (m.forall(x => okPairs.contains(x._2))) {
          Some(m)
        } else {
          None
        }
      }
      case _ => None
    }
    // How to end time here in scala ??
  }

  //TODO: could in theory do a merge without  a re parse, it would probably be faster too
  //TODO: We should profile this.  It still runs slower than I think it should.  
  def merge[A](a: (Int, Int), b: (Int, Int)): DagDfaFast[LABEL] = {

    val (aIn, aOut) = a
    val (bIn, bOut) = b

    val (newInputTree, inMap) = inputTree.merge(aIn, bIn)
    val (newoutputTree, outMap) = outputTree.merge(aOut, bOut)

    DagDfaFast(newInputTree, newoutputTree, okPairs.map(p => (inMap.getOrElse(p._1, p._1), outMap.getOrElse(p._2, p._2))))
  }

  //TODO: something like these should also be in the DagDfa class (or in a shared trait)
  //TODO: make name consistent with the tree version

  lazy val languageDescriptionCost = {
    val labelCost = log2(outputTree.labels.size)
    (okPairs.size * (log2(inputTree.ids.size.toDouble) + log2(outputTree.ids.size.toDouble)) + inputTree.cost + outputTree.cost
      - labelCost //  so we don't double count the shared informations
      )
  }

  def graphDescriptionCostGivenLanguage[A](g: Graph[A, DiEdge])(describe: A => LABEL): Double = {

    val reverse = reverseGraph(g)

    inputTree.conpressionCost(g)(describeg(g)(describe)) + outputTree.conpressionCost(reverse)(describeg(reverse)(describe))
  }

  //for now keeping it simple with just the cost of the trees
  def mdl[A](g: Graph[A, DiEdge])(describe: A => LABEL): Double = {

    languageDescriptionCost + graphDescriptionCostGivenLanguage(g)(describe)
  }

  /** returns the the different node types that can use this label */
  def getpossibleIds(label: LABEL): Set[(Int, Int)] = {

    val allLabelInIds = inputTree.transitions.filter(_.label == label).map(_.to)
    val allLabelOutIds = outputTree.transitions.filter(_.label == label).map(_.to)

    for (
      (in, out) <- okPairs if allLabelInIds.contains(in) && allLabelOutIds.contains(out)
    ) yield (in, out)
  }

  def getAncestors(inId: Int): Set[Int] = {
    //    all posible ansestors of a given ID (even when additional Id may also necisarily be created)

    //does scala really not have fixed point operator?
    //TODO: should probably live in the tree code?
    val startTime = System.currentTimeMillis().toDouble
    var reachableIds = Set[Int]()

    var newReachable = inputTree.transitions.filter(t => !t.from.toSet.intersect(reachableIds + inId).isEmpty).map(_.to)

    while (reachableIds != newReachable) {
      reachableIds = newReachable
      newReachable = inputTree.transitions.filter(t => !t.from.toSet.intersect(reachableIds + inId).isEmpty).map(_.to)
    }
    val endTime = System.currentTimeMillis().toDouble
    println("Time to get ancestors: " + (endTime - startTime))
    newReachable
  }

  def getDescendants(outId: Int) = {
    var reachableIds = Set[Int]()
    val startTime = System.currentTimeMillis().toDouble
    var newReachable = outputTree.transitions.filter(t => !t.from.toSet.intersect(reachableIds + outId).isEmpty).map(_.to)

    while (reachableIds != newReachable) {
      reachableIds = newReachable
      newReachable = outputTree.transitions.filter(t => !t.from.toSet.intersect(reachableIds + outId).isEmpty).map(_.to)
    }
    val endTime = System.currentTimeMillis().toDouble
    println("Time to get descendants: " + (endTime - startTime))
    newReachable
  }

}
