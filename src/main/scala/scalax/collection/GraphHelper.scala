package scalax.collection

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._

object GraphHelper {
  /** reverse the edges of a directed graph*/
  def reverseGraph[A](g: Graph[A, DiEdge]): Graph[A, DiEdge] = {
    val es = g.edges.map { case g.EdgeT(in, out) => (out.value ~> in.value) }
    Graph.from(g.nodes, es)
  }

  /** helper method to work around silly typing issues*/
  def describeNode[A, LABEL](g: Graph[A, DiEdge])(describe: A => LABEL)(n: g.NodeT): LABEL = {
    describe(n.value)
  }
}