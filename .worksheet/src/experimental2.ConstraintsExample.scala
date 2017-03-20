package experimental2

import scalax.collection.Graph
import scalax.collection.edge.Implicits._
//import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._

object ConstraintsExample {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(253); 
  println("Welcome to the Scala worksheet");$skip(78); 

  val g = Graph(0 ~> 1, 0 ~> 2,
    3 ~> 4,
    4 ~> 1,
    1 ~> 2, 2 ~> 5);System.out.println("""g  : scalax.collection.Graph[Int,scalax.collection.GraphEdge.DiEdge] = """ + $show(g ));$skip(15); val res$0 = 
  g.isDirected;System.out.println("""res0: Boolean = """ + $show(res$0))}
  //g.isAcyclic ???
//g.nodes.
}
