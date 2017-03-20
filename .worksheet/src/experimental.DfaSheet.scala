package experimental

import DeterministicRegularDagGrammar._
import scala.collection.Bag
import scala.collection.Bag._
import scala.collection.immutable.HashedBagConfiguration

//TODO: rename
object DfaSheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(256); 
  println("Welcome to the Scala worksheet");$skip(72); 

  //baggy nonsense
  implicit val m1 = Bag.configuration.compact[Int];System.out.println("""m1  : scala.collection.immutable.HashedBagConfiguration[Int] = """ + $show(m1 ));$skip(54); 
  implicit val m2 = Bag.configuration.compact[String];System.out.println("""m2  : scala.collection.immutable.HashedBagConfiguration[String] = """ + $show(m2 ))}

  //(Map[(Bag[ProductionId], Treminal), ProductionId], Map[ProductionId, Bag[ProductionId]])
  //val simpleDfa:Dfa[String, Int]  = (Map((Bag[Int](),"start")->0),Map())

}
