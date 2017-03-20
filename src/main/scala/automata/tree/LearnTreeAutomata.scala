package automata.tree

import scala.collection.immutable.MultiSet._
import scala.collection.immutable.Bag //suprisingly important to be explicit about this
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import TreeAutomata._

object LearnTreeAutomata {
  
  //TODO: TreeDfa[LABEL,Set[g.NodeT]] more acurite?
  //TODO: could even make more abstract, taking in an index function that considers the node values, could produce the sets, with a defualt set to id
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

    TreeDfa(transitions, roots) //alreaded minimized
  }

  //  def cost(t:TreeDfa[LABEL, _], g:Graph[_, DiEdge])(describe: g.NodeT => LABEL) => Double

  //TODO: make it stoppable, before it checks literally everything
  //TODO: do the red blue thing (though the example was medeocre on strings)
  def greedyLearn[LABEL](
    g: Graph[_, DiEdge])(
      describe: g.NodeT => LABEL) //(cost: (TreeDfa[LABEL, _], Graph[_, DiEdge]) => Double)
      : TreeDfa[LABEL, Int] = {
    require(g.isDirected)
    require(g.isAcyclic)

    val prefix = prefixDFA(g)(describe).withIntId //already minimized

    var knownCosts = Map[TreeDfa[LABEL, Int], Double](prefix -> prefix.mdl(g)(describe))

    var activeParents = Set[TreeDfa[LABEL, Int]](prefix)

    var lowestSeenCost = knownCosts.values.last

    while (!activeParents.isEmpty) {
      val cheapest = activeParents.minBy(knownCosts)

      if (knownCosts(cheapest) < lowestSeenCost) {
        println
        println("cost " + knownCosts(cheapest))
        println(cheapest.transitions.mkString("\n"))
        println(cheapest.roots)
        println
        lowestSeenCost = knownCosts(cheapest)
      } else {
        print(".")
      }

      val nfa = cheapest.toTreeNfa

      var newParents = Set[TreeDfa[LABEL, Int]]()

      for (
        pairs <- nfa.ids.subsets(2);
        List(a, b) = pairs.toList
      ) {
        val newDfa = nfa.merge(a, b)(a).toTreeDfa.minimize.withIntId

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