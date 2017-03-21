package automata.tree

import scala.collection.immutable.MultiSet._
import scala.collection.immutable.Bag //suprisingly important to be explicit about this
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import TreeAutomata._

//TODO: enforce minimal
/** a minimal tree dfa representation, parsed from levaes to roots*/
case class TreeDfa[LABEL, ID](transitions: Set[Transition[LABEL, ID]], override val roots: Set[ID]) extends TreeAutomata[LABEL, ID] {
  //    println
  //    println(this)

  require(!roots.isEmpty)

  require(reachable == transitions, s"\n$reachable\n$transitions\n${reachable.diff(transitions)}\n${transitions.diff(reachable)}")

  require(transitions.groupBy(t => (t.from, t.label)).mapValues(_.size).values.forall(_ == 1), transitions.groupBy(t => (t.from, t.label)).filter(_._2.size != 1))

  //TODO converting to a map to a map might be helpful
  lazy val transitionsToId = transitions.groupBy(t => (t.from, t.label)).mapValues(_.last.to)

  lazy val idToTransitions = transitions.groupBy(_.to)

  lazy val leaves = transitions.filter(_.from.isEmpty)

  require(!leaves.isEmpty)

  override lazy val ids = transitions.map(_.to)

  require(roots.subsetOf(ids))
  require(transitions.flatMap(_.from.toSet).subsetOf(ids))

  override lazy val labels = transitions.map(_.label)

  //cant assume it an id map will preserve the determinism
  override def map[NEW_LABEL, NEW_ID](
    fLabel: LABEL => NEW_LABEL,
    fId: ID => NEW_ID): TreeDfa[NEW_LABEL, NEW_ID] = TreeDfa[NEW_LABEL, NEW_ID](transitions.map(_.map(fLabel, fId)), roots.map(fId))

  override def mapId[NEW_ID](fId: ID => NEW_ID): TreeDfa[LABEL, NEW_ID] = map(identity, fId) //TODO: faster specialized?

  //any mapping of labels will preserve the determinism
  override def mapLabel[NEW_LABEL](fLabel: LABEL => NEW_LABEL) = TreeDfa[NEW_LABEL, ID](transitions.map(_.map(fLabel, identity)), roots)

  override def toTreeNfa = TreeNfa(transitions, roots)

  //TODO: require satisfiablility, non redundency

  def parse(g: Graph[_, DiEdge])(describe: g.NodeT => LABEL): Option[Map[g.NodeT, ID]] = {
    require(g.isDirected)
    require(g.isAcyclic)

    val Right(ts) = g.topologicalSort

    var map = Map[g.NodeT, ID]()

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

  //TODO: premote some of these to the trait

  //    //TODO: allow a parameter in the merging that tells how the new state should be iterperted
  //    def merege(a: ID, b: ID): TreeNfa[LABEL, ID] = {
  //
  //      ???
  //    }

  //TODO: again a big mess
  private def reachable: Set[Transition[LABEL, ID]] = {
    var reachableTransitions = transitions.filter(_.from.isEmpty)

    var reachableStates = reachableTransitions.map(_.to)

    while (true) { //TODO: could be more efiecient and cleaner, see:https://en.wikipedia.org/wiki/DFA_minimization
      val nextReachableTransitions = transitions.filter(_.from.toSet.subsetOf(reachableStates))
      val nextReachableStates = nextReachableTransitions.map(_.to)

      if (nextReachableTransitions == reachableTransitions) {
        return nextReachableTransitions
      } else {
        reachableTransitions = nextReachableTransitions
        reachableStates = nextReachableStates
      }
    }
    ???
  }

  override def toTreeDfa = this

  //TODO: could be more efiecient and cleaner, see:https://en.wikipedia.org/wiki/DFA_minimization, also the pruage stringology paper
  //TODO: helpful to output a TreeDfa[LABEL, Set[ID]], so the equivelence classes are explicit?
  def minimize: TreeDfa[LABEL, ID] = {

    import scala.collection.immutable.PairSet._

    //keep track of the non trivial euivelenve pairs!
    val terminal = roots.subsets(2).map(_.toSet2).toSet
    val nonTerminal = (ids -- roots).subsets(2).map(_.toSet2).toSet

    var oldPairs = Set[Set.Set2[ID]]()
    var pairs = terminal ++ nonTerminal

    //      println
    //      println(terminal)
    //      println
    //      println
    //      println(nonTerminal)
    //      println

    while (oldPairs != pairs) {
      oldPairs = pairs
      pairs = Set[Set.Set2[ID]]()

      //send every element to a representitive
      val oldMap = scala.collection.immutable.DisjointSets.mapToRepresentitive(oldPairs.map(_.toSet)).withDefault(identity)
      //        println
      //        println(ids.groupBy(oldMap).mkString("\n"))
      //        println
      //      //every transition under the equivelence
      //      val oldtransitions = transitions.map(_.mapId(oldMap))

      //for every old pair, keep it if it is indistinguishable under the existing equivelence relation
      for (
        pair <- oldPairs;
        List(a, b) = pair.toList
      ) {

        //                    val aTransitions = transitions.filter(_.from.contains(a)).map(_.mapId(oldMap))
        //                    val bTransitions = transitions.filter(_.from.contains(b)).map(_.mapId(oldMap))
        //                    

        //                    val aTransitions = transitions.filter(_.from.contains(a)).map(_.mapId(oldMap)).map(_.to)
        //                    val bTransitions = transitions.filter(_.from.contains(b)).map(_.mapId(oldMap)).map(_.to)

        val aTransitions = transitions.filter(_.to == a).map(_.mapId(oldMap))
        val bTransitions = transitions.filter(_.to == b).map(_.mapId(oldMap))

        //          println
        //          println(a)
        //          println(aTransitions)
        //          println(b)
        //          println(bTransitions)
        //          println

        if (aTransitions == bTransitions) {
          pairs += Set(a, b).toSet2
        }

      }

    }

    //
    //      var distinguisable = pairs.map(p => p -> p.map(id => roots.contains(id))).toMap.mapValues(_.reduce(_ == _))
    //
    //      var dependsOn = pairs.map(p => p -> Set[Set.Set2[ID]]())

    val map = scala.collection.immutable.DisjointSets.mapToRepresentitive(oldPairs.map(_.toSet)).withDefault(identity)

    //TODO: moniter for when this actually minimizes something

    this.mapId(map)
  }

  //TODO: should use ordering on labels to standardize, so this can get the normal form
  //TODO: rename
  def withIntId: TreeDfa[LABEL, Int] = {
    val idMap = ids.zipWithIndex.toMap

    mapId(idMap)
  }

  lazy val cost = {
    val idCost = log2(ids.size)
    val labelCost = log2(labels.size)

    //TODO: can factor some of this to make it faster
    idCost + roots.size.toDouble * idCost + labelCost + transitions.map(tr => tr.from.multiplicities.map(p => idCost + log2(p._2)).sum + labelCost + idCost).sum
  }

  def conpressionCost(g: Graph[_, DiEdge])(describe: g.NodeT => LABEL): Double =
    parse(g)(describe) match {
      case Some(map) => {
        //TODO: do something with this cooler with bags
        //          val idCounts = map.groupBy(_._2).mapValues(_.size)
        var total = 0.0

        for ((_, id) <- map) {
          //TODO: could cache this
          val r = if (roots.contains(id)) { 1 } else { 0 }
          total += log2(idToTransitions(id).size + r)
        }
        total
      }
      case None => Double.PositiveInfinity
    }

  def mdl(g: Graph[_, DiEdge])(describe: g.NodeT => LABEL): Double = {
    cost + conpressionCost(g)(describe)
  }

  //TODO: return a stream, make sure graphs are actually unique
  def enumerate(n: Int): Set[Graph[(Int, LABEL), DiEdge]] = {
    require(n >= 0)

    var map = ids.map(_ -> Array[Graph[(Int, LABEL), DiEdge]]()).toMap

    //{
    val reachableIds = map.filter(!_._2.isEmpty).keySet

    val activeTransitions = transitions.filter(_.from.toSet.subsetOf(reachableIds))

    //}

    ???
  }

}

  //TODO: normalized dfa