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
 * a dag grammar such that  each node is completely characterized by it's visible inpt and its visable output
 *  this is achieved by an input and output tree automata *
 */
case class DagDfaFast[LABEL](
    //what can deduce about a node from its inputs
    inputTree: TreeDfaFast[LABEL],
    //what can deduce about a node from its outputs
    outputTree: TreeDfaFast[LABEL],

    //TODO: could make this even more restricted with Map[LABEL, (IN_ID, OUT_ID)]
    okPairs: Set[(Int, Int)]) {
  

  def parse[A](g: Graph[A, DiEdge])(describe: A => LABEL): Option[Map[A, (Int, Int)]] = {

    val reverse = reverseGraph(g)

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

  //  TODO: mdl cost
  //for now keeping it simple with just the cost of the trees
  def mdl[A](g: Graph[A, DiEdge])(describe: A => LABEL): Double = {

    val reverse = reverseGraph(g)

    inputTree.mdl(g)(describeg(g)(describe)) + outputTree.mdl(reverse)(describeg(reverse)(describe)) + okPairs.size * (log2(inputTree.ids.size.toDouble) + log2(outputTree.ids.size.toDouble))
  }

}