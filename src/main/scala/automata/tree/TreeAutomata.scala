package automata.tree

import scala.collection.immutable.MultiSet._
import scala.collection.immutable.Bag //suprisingly important to be explicit about this
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import java.io.IOException
import java.io.ObjectOutputStream
import java.io.ObjectInputStream

@deprecated("move this to 'TreeDfaFast' ")
object TreeAutomata {

  case class Transition[LABEL, ID](val from: Bag[ID], val label: LABEL, val to: ID) extends Serializable {

    def map[NEW_LABEL, NEW_ID](
      fLabel: LABEL => NEW_LABEL,
      fId: ID => NEW_ID): Transition[NEW_LABEL, NEW_ID] = Transition(from.map(fId), fLabel(label), fId(to))

    def mapLabel[NEW_LABEL](fLabel: LABEL => NEW_LABEL): Transition[NEW_LABEL, ID] = map(fLabel, identity) //TODO: faster specialized?
    def mapId[NEW_ID](fId: ID => NEW_ID): Transition[LABEL, NEW_ID] = map(identity, fId) //TODO: faster specialized?

  }

}

