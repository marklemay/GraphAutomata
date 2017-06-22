package automata.tree

import scala.collection.immutable.MultiSet._
import scala.collection.immutable.Bag //suprisingly important to be explicit about this
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import TreeAutomata._
import math.Logs._

/** a tree DFA representation, parsed from leaves to roots, this is a version with many checks for well formedness*/
@deprecated("this should be removed, and everything should be done with 'TreeDfaFast' ")
case class TreeDfa[LABEL, ID](transitions: Set[Transition[LABEL, ID]], val roots: Set[ID]) {

  require(!roots.isEmpty, "roots must be defiend")

  require(transitions.groupBy(t => (t.from, t.label)).mapValues(_.size).values.forall(_ == 1), s"transition make should be deterministic, but is had multiple outputs on: ${transitions.groupBy(t => (t.from, t.label)).filter(_._2.size != 1)}")

  //TODO converting to a map to a map might be helpful
  lazy val transitionsToId = transitions.groupBy(t => (t.from, t.label)).mapValues(_.last.to)

  lazy val idToTransitions = transitions.groupBy(_.to)

  lazy val leaves = transitions.filter(_.from.isEmpty)

  require(!leaves.isEmpty)

  lazy val ids = transitions.map(_.to)

  require(roots.subsetOf(ids))
  require(transitions.flatMap(_.from.toSet).subsetOf(ids))

  lazy val labels = transitions.map(_.label)

  //can't assume it an id map will preserve the determinism
  def map[NEW_LABEL, NEW_ID](
    fLabel: LABEL => NEW_LABEL,
    fId: ID => NEW_ID): TreeDfa[NEW_LABEL, NEW_ID] = TreeDfa[NEW_LABEL, NEW_ID](transitions.map(_.map(fLabel, fId)), roots.map(fId))

  def mapId[NEW_ID](fId: ID => NEW_ID): TreeDfa[LABEL, NEW_ID] = map(identity, fId) //TODO: faster specialized?

  def mapLabel[NEW_LABEL](fLabel: LABEL => NEW_LABEL) = TreeDfa[NEW_LABEL, ID](transitions.map(_.map(fLabel, identity)), roots)

  def toFast: TreeDfaFast[LABEL] = {
    val TreeDfa(tran, rs) = this.withIntId

    TreeDfaFast(tran)
  }

  def toTreeDfa = this

  //TODO: should use ordering on labels to standardize, so this can get the normal form
  //TODO: rename
  def withIntId: TreeDfa[LABEL, Int] = {
    val idMap = ids.zipWithIndex.toMap

    mapId(idMap)
  }

}
