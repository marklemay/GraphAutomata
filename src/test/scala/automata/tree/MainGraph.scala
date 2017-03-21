package automata.tree

import scala.collection.immutable.MultiSet._
import scala.collection.immutable.Bag
import scalax.collection.Graph
import scalax.collection.edge.Implicits._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge.DiEdge
import automata.tree.TreeAutomata.Transition
import scalax.collection.GraphPredef

object MainGraph {

  def main(args: Array[String]): Unit = {
    //some examples enumeration style
    val g = Graph(
      1 ~> 2, 2 ~> 3)
      
      println(g)
      
      println(g.map(_.isNode))
      
//      g.map(f)
//      
      val es =g.edges.map{case g.EdgeT(in , out) => (out.value ~> in.value)}
    Graph.from(g.nodes, es)
    
    println(Graph.from(g.nodes, es))
//    
//    //(e=> (e._2, e._1))//(e._2 ~> e._1))// e._2 ~> e._1)
//      
//      println(es.map(p => (p._2, p._1) ))
//      
//      val pre:GraphPredef.Param[Int,List] = ???
//      println(g.map{case e:EdgeParam => {println (e.value.); e}
//      case a => a})
  }
}