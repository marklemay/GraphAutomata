package automata.tree

import scala.collection.immutable.MultiSet._
import scala.collection.immutable.Bag //suprisingly important to be explicit about this
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import TreeAutomata._

object LearnTreeAutomata {

  /**
   * creates a prefix tree DFA.  This is analogous to a prefix automata in DFA learning.
   */
  def prefixDFA[LABEL](g: Graph[_, DiEdge])(describe: g.NodeT => LABEL): TreeDfa[LABEL, g.NodeT] = {
    require(g.isDirected)
    require(g.isAcyclic)

    val Right(ts) = g.topologicalSort

    var map = Map[g.NodeT, g.NodeT]()

    //TODO: more efficient as map
    var transitions = Set[Transition[LABEL, g.NodeT]]()

    var roots = Set[g.NodeT]()

    for (
      node <- ts;
      label = describe(node)
    ) {
      val parentStates = node.inNeighbors.toBag.map(map)

      transitions.find(t => t.from == parentStates && t.label == label) match {
        case Some(Transition(_, _, id)) => {
          map += node -> id
        }
        case None => {
          transitions += Transition(parentStates, label, node)
          map += node -> node
        }
      }

      if (node.outDegree == 0) {
        roots += map(node)
      }

    }

    TreeDfa(transitions, roots) //already minimized
  }

}