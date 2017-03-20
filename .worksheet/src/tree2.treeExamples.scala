package tree2

import scalax.collection.Graph
import scalax.collection.edge.Implicits._
//import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._

object treeExamples {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(239); 
  println("Welcome to the Scala worksheet");$skip(60); 

  val g = Graph(-1 ~> 1, -2 ~> 2,
    1 ~> 3,
    2 ~> 3);System.out.println("""g  : scalax.collection.Graph[Int,scalax.collection.GraphEdge.DiEdge] = """ + $show(g ));$skip(33); 

  def describe(i: Int) = i < 0;System.out.println("""describe: (i: Int)Boolean""");$skip(23); val res$0 = 
  
  g.topologicalSort;System.out.println("""res0: tree2.treeExamples.g.CycleNodeOrTopologicalOrder = """ + $show(res$0))}
  
  //TreeAtomata.fromGraph(incoming, describe, topologicalOrder, leaves)
}
