package automata.tree

import scala.collection.immutable.MultiSet._
import scala.collection.immutable.Bag //suprisingly important to be explicit about this
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import TreeAutomata._

case class TreeNfa[LABEL, ID](transitions: Set[Transition[LABEL, ID]], roots: Set[ID]) extends TreeAutomata[LABEL, ID] {
  require(!roots.isEmpty)
  //TODO: test reachability

  override lazy val ids = transitions.map(_.to)
  override lazy val labels = transitions.map(_.label)

  override def map[NEW_LABEL, NEW_ID](
    fLabel: LABEL => NEW_LABEL,
    fId: ID => NEW_ID): TreeNfa[NEW_LABEL, NEW_ID] = TreeNfa[NEW_LABEL, NEW_ID](transitions.map(_.map(fLabel, fId)), roots.map(fId))

  override def mapLabel[NEW_LABEL](fLabel: LABEL => NEW_LABEL): TreeNfa[NEW_LABEL, ID] = map(fLabel, identity) //TODO: faster specialized?
  override def mapId[NEW_ID](fId: ID => NEW_ID): TreeNfa[LABEL, NEW_ID] = map(identity, fId) //TODO: faster specialized?

  override def toTreeNfa = this
  
  
  override def toFast:TreeDfaFast[LABEL] = ???

  // this is purely a convieniece method, more powerful merges can be achieveed through map
  //TODO: defualt parameter doesn;t exactly work?
  def merge(a: ID, b: ID)(into: ID = a): TreeNfa[LABEL, ID] = {
    def idMap(x: ID): ID = if (x == a || x == b) { into } else x

    mapId(idMap)
  }

  //    override def parse[N_ID](g: Graph[N_ID, DiEdge])(describe: g.NodeT => LABEL): Option[Map[g.NodeT, ID]] = ???

  private def singletonIds = this.mapId(x => Set(x))

  //    def combinations[A](bag:Bag[Set[A]]):Set[Bag[A]] =bag.lastOption match{
  //      
  //    }

  type NEW_ID = Set[ID]
  //SOOOOOO SLOOOOOOOOOW, algorithmically this can't be good, but 
  //TODO: first get sequences of bags directly (TODO: name? piles?)
  //TODO: in practice could be way faster, at the very least memoization and mapping will speed it up
  //TODO: can  optimize for NFAs that are really close to DFA
  def toTreeDfa: TreeDfa[LABEL, Set[ID]] = {

    val inSizes = transitions.map(_.from.size)

    var reachableIds = Set[NEW_ID]()

    var trans = Set[Transition[LABEL, NEW_ID]]()

    while (true) { //fixed point
      val oldIds = reachableIds

      for (inSize <- inSizes) {

        //          println(inSize)

        for (inputList <- reachableIds.sequences(inSize)) {
          val inputs = inputList.toBag

          //TODO: there has got to be a better way to do this!
          val reachableTransitions = transitions.filter(_.from.size == inSize).filter(tr => isSatisfiable(inputList, tr.from)((a, b) => a.contains(b)))

          val protoTransitions = reachableTransitions.groupBy(_.label).mapValues(_.map(_.to))

          for ((label, dest) <- protoTransitions) {
            reachableIds += dest
            trans += Transition(inputs, label, dest)

          }

        }

      }

      if (oldIds == reachableIds) {

        return TreeDfa(trans, oldIds.filter(x => !(x.intersect(roots)).isEmpty))

      }
    }

    ???
  }
}