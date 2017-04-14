package automata.tree

import scala.collection.immutable.MultiSet._
import scala.collection.immutable.Bag //suprisingly important to be explicit about this
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge

object TreeAutomata {
  //TODO: move this
   def log2(x: Double) = if (x <= 0d) { 0d } else { Math.log10(x) / Math.log10(2.0) }

  //TODO: do a Bag version
  //TODO: avoid this entirely?
  //TODO: momoize
  def isSatisfiable[A, B](lsA: List[A], lsB: Bag[B])(sat: (A, B) => Boolean): Boolean = {
    require(lsA.size == lsB.size, s"lsA.size = ${lsA.size} != ${lsB.size} = lsB.size")
    if (lsA.isEmpty && lsB.isEmpty) {
      return true
    } else {

      val a :: aRest = lsA
      for (b <- lsB) {
        if (sat(a, b) && isSatisfiable(aRest, lsB - b)(sat)) {
          return true
        }
      }
      return false
    }
  }

  case class Transition[LABEL, ID](from: Bag[ID], label: LABEL, to: ID) {

    def map[NEW_LABEL, NEW_ID](
      fLabel: LABEL => NEW_LABEL,
      fId: ID => NEW_ID): Transition[NEW_LABEL, NEW_ID] = Transition(from.map(fId), fLabel(label), fId(to))

    def mapLabel[NEW_LABEL](fLabel: LABEL => NEW_LABEL): Transition[NEW_LABEL, ID] = map(fLabel, identity) //TODO: faster specialized?
    def mapId[NEW_ID](fId: ID => NEW_ID): Transition[LABEL, NEW_ID] = map(identity, fId) //TODO: faster specialized?

  }
  
  
   //A trait to do all the work common to both NFA and DFA

  //TODO: should tree automata be ID agnostic?
  trait TreeAutomata[LABEL, ID] { //TODO: extends Tree lang

    val roots: Set[ID]
    val ids: Set[ID]
    val labels: Set[LABEL]

    def map[NEW_LABEL, NEW_ID](
      fLabel: LABEL => NEW_LABEL,
      fId: ID => NEW_ID): TreeAutomata[NEW_LABEL, NEW_ID]

    def mapLabel[NEW_LABEL](fLabel: LABEL => NEW_LABEL): TreeAutomata[NEW_LABEL, ID] = map(fLabel, identity)
    def mapId[NEW_ID](fId: ID => NEW_ID): TreeAutomata[LABEL, NEW_ID] = map(identity, fId)

    //TODO: make it a full monad? bimonad? there can never be too many categories

    //TODO: can get parsing in terms of a general functional intrface, and then build the specific graph parser out of that!
    //    def parse(g: Graph[_, DiEdge])(describe: g.NodeT => LABEL): Option[Map[g.NodeT, ID]]
    //
    def toTreeDfa: TreeDfa[LABEL, _]

    def toTreeNfa: TreeNfa[LABEL, ID]

    //    def compressIDs: TreeDfa[LABEL, ID]

    def toFast: TreeDfaFast[LABEL]
  }

}

