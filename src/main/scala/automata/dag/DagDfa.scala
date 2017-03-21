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

    //TODO: could make this even more restricted with Map[LABEL, (IN_ID, OUT_ID)]
    okPairs: Set[(IN_ID, OUT_ID)]) {
  //TODO: reachability and other sanity checks

  //TODO: parse
  def parse[A](g: Graph[A, DiEdge])(describe: A => LABEL): Option[Map[A, (IN_ID, OUT_ID)]] = {
    require(g.isDirected)
    require(g.isAcyclic)

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

  //TODO: merge
  //TODO: could in theory do a merge without  a re parse, it would probably be faster too
  def merge[A](a: (IN_ID, OUT_ID), b: (IN_ID, OUT_ID))(g: Graph[A, DiEdge])(describe: A => LABEL): DagDfa[LABEL, _, _] = {
    val (aIn, aOut) = a
    val (bIn, bOut) = b

    val newInputTree = inputTree.toTreeNfa.merge(aIn, bIn)(aIn).toTreeDfa.minimize.withIntId
    val newoutputTree = outputTree.toTreeNfa.merge(aOut, bOut)(aOut).toTreeDfa.minimize.withIntId

    val reverse = reverseGraph(g)
    
//    println
//    println(g)
//    println(reverse)
//    println

    val Some(inMap) = newInputTree.parse(g)(describeg(g)(describe))
    val Some(outMap) = newoutputTree.parse(reverse)(describeg(reverse)(describe))

    val ins = inMap.map(p => p._1.value -> p._2)
    val outs = outMap.map(p => p._1.value -> p._2)

    val m = g.nodes.map(_.value).map(a => a -> (ins(a), outs(a))).toMap

    DagDfa(newInputTree, newoutputTree, m.values.toSet)
  }

  //  TODO: mdl cost
  //for now keeping it simple with just the cost of the trees
  def mdl[A](g: Graph[A, DiEdge])(describe: A => LABEL): Double = {

    val reverse = reverseGraph(g)

    inputTree.mdl(g)(describeg(g)(describe)) + outputTree.mdl(reverse)(describeg(reverse)(describe))
  }

  //TODO: rename
  def withIdIndex[A](g: Graph[A, DiEdge])(describe: A => LABEL): DagDfa[LABEL, Int, Int] = {

    val newInputTree = inputTree.withIntId
    val newoutputTree = outputTree.withIntId

    //The following comes up a lot so factor it out into a helper method?
    val reverse = reverseGraph(g)

    val Some(inMap) = newInputTree.parse(g)(describeg(g)(describe))
    val Some(outMap) = newoutputTree.parse(reverse)(describeg(reverse)(describe))

    val ins = inMap.map(p => p._1.value -> p._2)
    val outs = outMap.map(p => p._1.value -> p._2)

    val m = g.nodes.map(_.value).map(a => a -> (ins(a), outs(a))).toMap

    DagDfa(newInputTree, newoutputTree, m.values.toSet)

  }

  //    parse(g)(describe) match {
  //      case Some(map) => {
  //        //TODO: do something with this cooler with bags
  //        //          val idCounts = map.groupBy(_._2).mapValues(_.size)
  //        var total = 0.0
  //
  //        for ((_, id) <- map) {
  //          //TODO: could cache this
  //          val r = if (roots.contains(id)) { 1 } else { 0 }
  //          total += log2(idToTransitions(id).size + r)
  //        }
  //        total
  //      }
  //      case None => Double.PositiveInfinity
  //    }

  //  TODO: can factor in satisfiability

}