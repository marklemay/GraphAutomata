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


//TODO: not thrilled with this name
/**
 * a dag grammar such that  each node is completely characterized by it's visible inpt and its visable output
 *  this is achieved by an input and output tree automata *
 */
case class DagDfa[LABEL, IN_ID, OUT_ID](
    //what can deduce about a node from its inputs
    inputTree: TreeDfa[LABEL, IN_ID],
    //what can deduce about a node from its outputs
    outputTree: TreeDfa[LABEL, OUT_ID],

    okPairs: Set[(IN_ID, OUT_ID)]) {
  //TODO: reachability and other sanity checks




  
  
  //TODO: parse
  def parse[A](g: Graph[A, DiEdge])(describe: A => LABEL): Option[Map[A, (IN_ID, OUT_ID)]] = {
    require(g.isDirected)
    require(g.isAcyclic)

    val reverse = reverseGraph(g)
    

    

    (inputTree.parse(g)(describeg(g)(describe)), outputTree.parse(reverse)(describeg(reverse)(describe)) ) match {
      case (Some(inMap), Some(outMap)) => {
        val ins = inMap.map(p=> p._1.value -> p._2)
        val outs= outMap.map(p=> p._1.value -> p._2)
        
        val m =g.nodes.map(_.value).map(a=> a->(ins(a),outs(a))).toMap
        
        if(m.forall(x=> okPairs.contains(x._2))){
          Some(m)
        }else{
          None
        }
      }
      case _ => None
    }
  }

  //TODO: merge
  def merge(a: (IN_ID, OUT_ID), b: (IN_ID, OUT_ID)): DagDfa[LABEL, IN_ID, OUT_ID] = {

    ???
  }

  //  TODO: mdl cost
  //  TODO: can factor in satisfiability

}