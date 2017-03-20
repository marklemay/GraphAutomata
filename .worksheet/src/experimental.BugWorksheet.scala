package experimental

import scala.collection.Bag

object BugWorksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(118); 
  println("Welcome to the Scala worksheet");$skip(44); 

  val m1 = Bag.configuration.compact[Int];System.out.println("""m1  : scala.collection.immutable.HashedBagConfiguration[Int] = """ + $show(m1 ))}
}
