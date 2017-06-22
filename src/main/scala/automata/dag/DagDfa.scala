package automata.dag
import scala.collection.immutable.MultiSet._
import scala.collection.immutable.Bag //suprisingly important to be explicit about this
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.edge.Implicits._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef
import scalax.collection.GraphHelper._
import LearnDeterministicDag._
import automata.tree.TreeDfaFast
import math.Logs._

import java.io.IOException
import java.io.ObjectOutputStream

/**
 * Defines a "DAG grammar" such that each node is completely characterized by it's visible input and output trees.
 */
case class DagDfa[LABEL](
    //what can deduce about a node from its inputs
    inputTree: TreeDfaFast[LABEL],
    //what can deduce about a node from its outputs
    outputTree: TreeDfaFast[LABEL],

    //TODO: rename to nodeTypes? states?  it's good to think of these as the "type" of node, a label can belong to multiple types with additional conditions on input and output.
    okPairs: Set[(Int, Int)]) {

  def parse[A](g: Graph[A, DiEdge])(describe: A => LABEL): Option[Map[A, (Int, Int)]] = {

    val reverse = reverseGraph(g)

    (inputTree.parse(g)(describeNode(g)(describe)), outputTree.parse(reverse)(describeNode(reverse)(describe))) match {
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
  }

  //TODO: could in theory do a merge without  a re parse, it would probably be faster too
  //TODO: We should profile this.  It still runs slower than I think it should.  
  def merge[A](a: (Int, Int), b: (Int, Int)): DagDfa[LABEL] = {

    val (aIn, aOut) = a
    val (bIn, bOut) = b

    val (newInputTree, inMap) = inputTree.merge(aIn, bIn)
    val (newoutputTree, outMap) = outputTree.merge(aOut, bOut)

    DagDfa(newInputTree, newoutputTree, okPairs.map(p => (inMap.getOrElse(p._1, p._1), outMap.getOrElse(p._2, p._2))))
  }

  lazy val descriptionCost = {
    val labelCost = log2(outputTree.labels.size)

    (okPairs.size * (log2(inputTree.ids.size.toDouble) + log2(outputTree.ids.size.toDouble)) + inputTree.descriptionCost + outputTree.descriptionCost
      - labelCost //  so we don't double count the shared informations
      )
  }

  def graphDescriptionCostGivenThis[A](g: Graph[A, DiEdge])(describe: A => LABEL): Double = {

    val reverse = reverseGraph(g)

    //note: this is a little arbitrary, for now keeping it simple with just the cost of the trees
    inputTree.graphDescriptionCostGivenThis(g)(describeNode(g)(describe)) + outputTree.graphDescriptionCostGivenThis(reverse)(describeNode(reverse)(describe))
  }

  /** Minimum Description Length */
  def mdl[A](g: Graph[A, DiEdge])(describe: A => LABEL): Double = {

    descriptionCost + graphDescriptionCostGivenThis(g)(describe)
  }

  /** returns the the different node types that can use this label */
  def getpossibleIds(label: LABEL): Set[(Int, Int)] = {

    val allLabelInIds = inputTree.transitions.filter(_.label == label).map(_.to)
    val allLabelOutIds = outputTree.transitions.filter(_.label == label).map(_.to)

    for (
      (in, out) <- okPairs if allLabelInIds.contains(in) && allLabelOutIds.contains(out)
    ) yield (in, out)
  }

}
