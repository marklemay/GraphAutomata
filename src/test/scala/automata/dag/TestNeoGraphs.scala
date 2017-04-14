package automata.dag
import neo4j_scala_graph.NeoData._
import org.neo4j.driver.v1.{ AuthTokens, GraphDatabase }
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import org.junit.Ignore

class TestNeoGraphs extends AssertionsForJUnit {

  sealed trait Desc
  case class Artifact() extends Desc
  case class Process(name: String) extends Desc

  case class EdgeDesc(t: String) extends Desc

  def describe(nd: NeoData): Desc = nd match {
    case NeoNode(_, labels, _) if labels == Set("Artifact") => Artifact() //TODO: add path?
    case NeoNode(_, labels, prop) if labels == Set("Process") => Process(prop.getOrElse("name", "???").asInstanceOf[String]) //TODO: Option
    case NeoRel(_, t, _) => EdgeDesc(t)

  }

  @Ignore
  @Test
  def learnNeo4jOnlyActivities: Unit = {
    println("wajih")
    val driver = GraphDatabase.driver("bolt://localhost", AuthTokens.basic("neo4j", "n"))
    println("wajihdone")
    val session = driver.session();
    val g = toDiGraph(run(session)("MATCH (n)-[r]-()  where n.type='Process' AND r.type='WasTriggeredBy' RETURN n,r;"))
    println(LearnDeterministicDag.greedyLearn(g, 10)(describe))
  }

  
  // bigger graphs, may need to give the jvm needs more memory
  @Test
  def learnNeo4j: Unit = {
    {
      val mb = 1024*1024;
      val run = Runtime.getRuntime();
      println(run.totalMemory() / mb)
    }
    
    
    println("wajih")
    // I REALLY hate how the prov arrows go in the opposite direction of cuasality, unlike literally everything ever.  who thought this was a good idea?
    val driver = GraphDatabase.driver("bolt://localhost", AuthTokens.basic("neo4j", "n"))
    println("wajihdone")
    val session = driver.session();
    val g = fullGraph(session)
    //    println(g.mkString(sep)
    println(LearnDeterministicDag.greedyLearn(g, 120)(describe))
  }
}