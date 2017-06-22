package automata.tree

import scala.collection.immutable.MultiSet._
import scala.collection.immutable.Bag //suprisingly important to be explicit about this
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import TreeAutomata._
import java.io.IOException
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import math.Logs._

/** A minimal tree DFA representation, a tree DFA optimized for speed */
//TODO: make value class to save on allocation
case class TreeDfaFast[LABEL](val transitions: Set[Transition[LABEL, Int]]) {

  /** map from transitions to values */
  lazy val transitionsToId = transitions.groupBy(t => (t.from, t.label)).mapValues(_.last.to)

  /** get all the transitions that could map to this value */
  lazy val idToTransitions = transitions.groupBy(_.to)

  /** all possible states */
  lazy val ids = transitions.map(_.to)

  /** all possible labels */
  lazy val labels = transitions.map(_.label)

  /** all possible labels */
  def parse(g: Graph[_, DiEdge])(describe: g.NodeT => LABEL): Option[Map[g.NodeT, Int]] = {
    require(g.isDirected, "graph must be directed")
    require(g.isAcyclic, "graph must be acyclic")

    val Right(ts) = g.topologicalSort

    var map = Map[g.NodeT, Int]()

    for (
      node <- ts;
      label = describe(node)
    ) {
      val parentStates = node.inNeighbors.toBag.map(map)
      transitionsToId.get((parentStates, label)) match {
        case Some(id) => map += (node -> id)
        case None => return None
      }
    }

    Some(map)
  }

  private def nonDeterministicMerge(trans: Set[Transition[LABEL, Int]])(a: Int, b: Int): (Set[Transition[LABEL, Int]]) = {

    val min = Math.min(a, b)
    val max = Math.max(a, b)

    trans.map(_.mapId(id => if (id == max) { min } else { id }))
  }

  // don't need to find all the nondeterminism
  private def findFirstNonDeterminism(trans: Set[Transition[LABEL, Int]]): Option[(Int, Int)] = {
    var seen = Map[(Bag[Int], LABEL), Int]()

    //TODO: this search could be faster if informed by prevous merges
    for (Transition(from, label, to) <- trans) {
      val in = (from, label)

      if (seen.contains(in)) {
        return Some(seen(in), to)
      } else {
        seen += in -> to
      }

    }
    return None
  }

  //TODO: also take an efficient order, choose the smaller of the 2, this will make the representations unique
  def merge(a: Int, b: Int): (TreeDfaFast[LABEL], Map[Int, Int]) = {

    //using max and min make mereged trees comparable
    var map = Map(Math.max(a, b) -> Math.min(a, b))

    var newTrans = nonDeterministicMerge(transitions)(a, b)

    var det = findFirstNonDeterminism(newTrans)

    while (det.isDefined) {
      val Some((x, y)) = det

      val max = Math.max(x, y)
      val min = Math.min(x, y)

      map = map.map(p => if (p._2 == max) { p._1 -> min } else { p })

      map += max -> min

      newTrans = nonDeterministicMerge(newTrans)(x, y)

      det = findFirstNonDeterminism(newTrans)

    }

    (TreeDfaFast(newTrans), map)
  }

  /** the approxamate cost in bits of the tree DFA */
  lazy val descriptionCost = {
    val idCost = log2(ids.size)
    val labelCost = log2(labels.size)

    //TODO: can factor some of this to make it faster
    idCost + labelCost + transitions.map(tr => tr.from.multiplicities.map(p => idCost + log2(p._2)).sum + labelCost + idCost).sum
  }

  /** the approxamate cost in bits of a DAG given this tree DFA */
  def graphDescriptionCostGivenThis(g: Graph[_, DiEdge])(describe: g.NodeT => LABEL): Double =
    parse(g)(describe) match {
      case Some(map) => {
        //TODO: do something with this cooler with bags
        //          val idCounts = map.groupBy(_._2).mapValues(_.size)
        var total = 0.0

        for ((_, id) <- map) {
          //TODO: could cache this
          total += log2(idToTransitions(id).size)
        }
        total
      }
      case None => Double.PositiveInfinity
    }

  /** Minimum Description Length */
  def mdl(g: Graph[_, DiEdge])(describe: g.NodeT => LABEL): Double = {
    descriptionCost + graphDescriptionCostGivenThis(g)(describe)
  }

}
