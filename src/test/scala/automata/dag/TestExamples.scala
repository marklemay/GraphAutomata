package automata.dag
import org.junit.Test
import org.junit.Assert.assertTrue
import scala.collection.immutable.MultiSet._
import scala.collection.immutable.Bag //suprisingly important to be explicit about this
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import automata.tree.TreeDfa
import scalax.collection.edge.Implicits._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef
import automata.tree.LearnTreeAutomata
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.Ignore

@Test
class TestExamples extends AssertionsForJUnit {

  /** allows us to concisely label graph nodes with prefixes */
  def describe(s: String) = s.split('_').head
  //TODO, describe with primes

  @Test
  def learnTrivalString: Unit = {

    //TODO need to figure out hyper edges to make these graph literals more concise
    val g = Graph(
      "a_0_0" ~> "a_0_1",

      "a_1_0" ~> "a_1_1", "a_1_1" ~> "a_1_2",

      "a_2_0" ~> "a_2_1", "a_2_1" ~> "a_2_2", "a_2_2" ~> "a_2_3")
    //    println(g)

    val detdag = LearnDeterministicDag.greedyLearn(g, 10)(describe)
    //    println(detdag)

    assert(detdag.parse(g)(describe).isDefined, "should be able to parse itself")

    val g2 = Graph("a_3_0" ~> "a_3_1", "a_3_1" ~> "a_3_2", "a_3_2" ~> "a_3_3", "a_3_3" ~> "a_3_4")
    assert(detdag.parse(g2)(describe).isDefined, "should be able to parse a similar but unseen string")

  }

  @Test
  def learnConcretePattern: Unit = {

    //TODO need to figure out hyper edges to make these graph literals more concise
    val g = Graph(
      "a_0" ~> "b_0", "b_0" ~> "c_0", "c_0" ~> "d_0",

      "a_1" ~> "b_1", "b_1" ~> "c_1", "c_1" ~> "d_1",

      "a_2" ~> "b_2", "b_2" ~> "c_2", "c_2" ~> "d_2",

      "a_3" ~> "b_3", "b_3" ~> "c_3", "c_3" ~> "d_3",

      "a_4" ~> "b_4", "b_4" ~> "c_4", "c_4" ~> "d_4")
    //    println(g)

    val detdag = LearnDeterministicDag.greedyLearn(g, 10)(describe)
    //TODO: when the code settles down we can make sure this converges to the expected litteral
    //    println
    //    println(detdag)

    assert(detdag.parse(g)(describe).isDefined, "should be able to parse itself")

    val g2 = Graph("a" ~> "b", "b" ~> "c", "c" ~> "d")
    assert(detdag.parse(g2)(describe).isDefined, "should be able to parse a similar but unseen string")

    val bad1 = Graph("a" ~> "b", "b" ~> "c", "c" ~> "a_1")

    //    println(detdag.parse(bad1)(describe))

    assert(!detdag.parse(bad1)(describe).isDefined, "should NOT be able to parse a string with a different patern")
  }

  @Test
  def learnRepeadtedStringPattern: Unit = {

    //TODO need to figure out hyper edges to make these graph literals more concise
    val g = Graph(
      "a_0" ~> "b_0", "b_0" ~> "a_1",
      "a_1" ~> "b_1", "b_1" ~> "a_2",
      "a_2" ~> "b_2", "b_2" ~> "a_3",
      "a_3" ~> "b_3", "b_3" ~> "a_4",
      "a_4" ~> "b_4", "b_4" ~> "a_5")

    assert(g.isConnected)
    //    println(g)

    val detdag = LearnDeterministicDag.greedyLearn(g, 10)(describe)
    //TODO: when the code settles down we can make sure this converges to the expected litteral
    //    println
    //    println(detdag)

    assert(detdag.parse(g)(describe).isDefined, "should be able to parse itself")

    val g2 = Graph(
      "a_0" ~> "b_0", "b_0" ~> "a_1",
      "a_1" ~> "b_1", "b_1" ~> "a_2",
      "a_2" ~> "b_2", "b_2" ~> "a_3",
      "a_3" ~> "b_3", "b_3" ~> "a_4",
      "a_4" ~> "b_4", "b_4" ~> "a_5",
      "a_5" ~> "b_5", "b_5" ~> "a_6")
    assert(detdag.parse(g2)(describe).isDefined, "should be able to parse a similar but unseen string")

    val bad1 = Graph(
      "a_0" ~> "b_0", "b_0" ~> "a_1",
      "a_1" ~> "b_1", "b_1" ~> "a_2",
      "a_2" ~> "b_2", "b_2" ~> "a_3",
      "a_3" ~> "b_3", "b_3" ~> "a_4",
      "a_4" ~> "b_4", "b_4" ~> "b_5")

    //    println(detdag.parse(bad1)(describe))

    assert(!detdag.parse(bad1)(describe).isDefined, "should NOT be able to parse a string that does not match the underlieing patern")
  }

  @Test
  def learnDisjointConcreteStrings: Unit = {

    //TODO need to figure out hyper edges to make these graph literals more concise
    val g = Graph(
      "a_0" ~> "b_0", "b_0" ~> "c_0", "c_0" ~> "d_0",

      "a_1" ~> "b_1", "b_1" ~> "c_1", "c_1" ~> "d_1",

      "w_2" ~> "x_2", "x_2" ~> "y_2", "y_2" ~> "z_2",
      "w_3" ~> "x_3", "x_3" ~> "y_3", "y_3" ~> "z_3")
    //    println(g)

    val detdag = LearnDeterministicDag.greedyLearn(g, 10)(describe)
    //TODO: when the code settles down we can make sure this converges to the expected litteral
    //    println
    //    println(detdag)

    assert(detdag.parse(g)(describe).isDefined, "should be able to parse itself")

    val g2 = Graph("a" ~> "b", "b" ~> "c", "c" ~> "d")
    assert(detdag.parse(g2)(describe).isDefined, "should be able to parse a similar but unseen string")

    val g3 = Graph("w" ~> "x", "x" ~> "y", "y" ~> "z")
    assert(detdag.parse(g3)(describe).isDefined, "should be able to parse a similar but unseen string")

    val bad1 = Graph("a" ~> "b", "b" ~> "c", "c" ~> "a_1")

    //    println(detdag.parse(bad1)(describe))

    assert(!detdag.parse(bad1)(describe).isDefined, "should NOT be able to parse a string with a different patern")
  }

  //TODO: some of these may need the learning code to be more sensitive to work

  //TODO: branching trees
  //TODO: concrete trees
  //TODO: branching trees with some implicit associations (all the leaves have to be consistant)

  @Test
  def learnRepeatedDimonds: Unit = {

    //TODO need to figure out hyper edges to make these graph literals more concise
    //TODO: rename to be more consitent with the paper
    val g = Graph(
      "a_0" ~> "b_0", "b_0" ~> "a_1", "a_1" ~> "b_1", "b_1" ~> "a_2", "a_2" ~> "b_3", "b_3" ~> "a_3",
      "a_0" ~> "d_0", "d_0" ~> "a_1", "a_1" ~> "d_1", "d_1" ~> "a_2", "a_2" ~> "d_3", "d_3" ~> "a_3",

      "a_4" ~> "b_4", "b_4" ~> "a_5", "a_5" ~> "b_5", "b_5" ~> "a_6", "a_6" ~> "b_6", "b_6" ~> "a_7", "a_7" ~> "b_7", "b_7" ~> "a_8",
      "a_4" ~> "d_4", "d_4" ~> "a_5", "a_5" ~> "d_5", "d_5" ~> "a_6", "a_6" ~> "d_6", "d_6" ~> "a_7", "a_7" ~> "d_7", "d_7" ~> "a_8")
    assert(g.isDirected)
    assert(g.isAcyclic)
    //    assert(g.isConnected)

    //    println(g)

    val detdag = LearnDeterministicDag.greedyLearn(g, 10)(describe)
    //TODO: when the code settles down we can make sure this converges to the expected litteral
    println
    println(detdag)

    assert(detdag.parse(g)(describe).isDefined, "should be able to parse itself")

    val g2 = Graph(
      "a_0" ~> "b_0", "b_0" ~> "a_1", "a_1" ~> "b_1", "b_1" ~> "a_2", "a_2" ~> "b_3", "b_3" ~> "a_3", "a_3" ~> "b_4", "b_4" ~> "a_5", "a_5" ~> "b_5", "b_5" ~> "a_6",
      "a_0" ~> "d_0", "d_0" ~> "a_1", "a_1" ~> "d_1", "d_1" ~> "a_2", "a_2" ~> "d_3", "d_3" ~> "a_3", "a_3" ~> "d_4", "d_4" ~> "a_5", "a_5" ~> "d_5", "d_5" ~> "a_6")
    assert(detdag.parse(g2)(describe).isDefined, "should be able to parse a similar but unseen dag")

    val bad1 = Graph(
      "a_0" ~> "b_0", "b_0" ~> "a_1", "a_1" ~> "b_1", "b_1" ~> "a_2", "a_2" ~> "b_3", "b_3" ~> "a_3",
      "a_0" ~> "d_0", "d_0" ~> "a_1", "a_1" ~> "d_1", "d_1" ~> "a_2")

    //    println(detdag.parse(bad1)(describe))

    assert(!detdag.parse(bad1)(describe).isDefined, "should NOT be able to parse a dag with a different patern")

    val bad2 = Graph(
      "a_0" ~> "b_0", "b_0" ~> "a_1", "a_1" ~> "d_1", "d_1" ~> "a_2",
      "a_0" ~> "d_0", "d_0" ~> "a_1", "a_1" ~> "d_2", "d_2" ~> "a_2")

    //    println(detdag.parse(bad1)(describe))

    assert(!detdag.parse(bad2)(describe).isDefined, "should NOT be able to require different ")

  }

  @Test
  def learnPartlyRepeatingDags: Unit = {

    //TODO need to figure out hyper edges to make these graph literals more concise
    val g = Graph(
      "start_0" ~> "single_0", "single_0" ~> "end_0",
      "start_0" ~> "..._0", "..._0" ~> "end_0",

      "start_1" ~> "single_1", "single_1" ~> "end_1",
      "start_1" ~> "..._1", "..._1" ~> "..._'1", "..._'1" ~> "end_1",

      "start_2" ~> "single_2", "single_2" ~> "end_2",
      "start_2" ~> "..._2", "..._2" ~> "..._'2", "..._'2" ~> "..._''2", "..._''2" ~> "end_2")
    assert(g.isDirected)
    assert(g.isAcyclic)

    //    println(g)

    val detdag = LearnDeterministicDag.greedyLearn(g, 10)(describe)
    //TODO: when the code settles down we can make sure this converges to the expected litteral
    println
    println(detdag)

    assert(detdag.parse(g)(describe).isDefined, "should be able to parse itself")

    val g2 = Graph(
      "start" ~> "single", "single" ~> "end",
      "start" ~> "...", "..." ~> "..._'", "..._'" ~> "..._''", "..._''" ~> "..._'''", "..._'''" ~> "end")
    assert(detdag.parse(g2)(describe).isDefined, "should be able to parse a similar but unseen dag")

    val bad1 = Graph(

      "start" ~> "single", "single" ~> "single_'", "single_'" ~> "end",
      "start" ~> "...", "..." ~> "..._'", "..._'" ~> "..._''", "..._''" ~> "end")

    //    println(detdag.parse(bad1)(describe))

    assert(!detdag.parse(bad1)(describe).isDefined, "should NOT be able to parse a dag with a different patern")
  }

  //TODO: concrete dags
  //TODO: branching dags
  //TODO: the 3 sat grammar for fun
}