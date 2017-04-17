package automata.dag
import java.io._
import neo4j_scala_graph.NeoData._
import org.neo4j.driver.v1.{ AuthTokens, GraphDatabase }
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import automata.tree.TreeAutomata._
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge // labeled directed edge
import scalax.collection.edge.Implicits._ // shortcuts
import org.neo4j.driver.v1.StatementResult

import org.junit.Ignore

class TestNeoGraphs extends AssertionsForJUnit {

  sealed trait Desc
  case class Artifact(name: String) extends Desc
  case class Process(name: String) extends Desc

  case class EdgeDesc(t: String) extends Desc

  def writeGraphInitial(g: Graph[NeoData, DiEdge]) = {
    val fos = new FileOutputStream("initialgraph.obj")
    val oos = new ObjectOutputStream(fos)
    oos.writeObject(g)
    oos.close
  }

  def writeGraphFinal(g: Graph[NeoData, DiEdge]) = {
    val fos = new FileOutputStream("finalgraph.obj")
    val oos = new ObjectOutputStream(fos)
    oos.writeObject(g)
    oos.close
  }

  def createGraphFromDag(dagdfa: DagDfaFast[_]) : Graph[NeoData, DiEdge] = {
    val list_trans = dagdfa.inputTree.transitions
    println(list_trans)
    var idtoLabel = Map[Int, Set[String]]()
    var g = Graph[NeoData, DiEdge]()
    for ( li <- list_trans){
      if( li.label.isInstanceOf[Artifact]) {
        idtoLabel += li.to -> Set("Artifact")
      }
      if( li.label.isInstanceOf[Process]) {
        idtoLabel += li.to -> Set("Process")
      }
    }
    var nodes = Set[NeoData]()
    var edges = Set[DiEdge[NeoNode]]()
    for ( item <- list_trans){
      var node = NeoNode(item.to, idtoLabel(item.to), Map())
      // var node = NeoNode(item.to, Set("???"), Map())
      nodes += node
      for ( ances <- item.from){
        var node_2 = NeoNode(ances, idtoLabel(item.to), Map())
        nodes += node_2
        edges += node_2 ~> node
      }
    }
    for (edge <- edges){
      g += edge
    }
    return g
  }

  def describe(nd: NeoData): Desc = nd match {
    case NeoNode(_, labels, prop) if labels == Set("Artifact") => {
      val path=prop.getOrElse("path", "").asInstanceOf[String]
      if (path.startsWith("/usr/lib")){
        return Artifact("/usr/lib/")
      }else if (path.startsWith("/etc/")){
        return Artifact("/etc/")
      }else if (path.startsWith("/home/")){
        return Artifact("/home/")
      }else if (path.startsWith("/usr/bin")){
        return Artifact("/usr/bin/")
      }else if (path.startsWith("/usr/share")){
        return Artifact("/usr/share/")
      }
      return Artifact("")
    }
    case NeoNode(_, labels, prop) if labels == Set("Process") => {
      Process(prop.getOrElse("name", "???").asInstanceOf[String])
    }
    case NeoRel(_, t, _) => EdgeDesc(t)

  }

  def describe_original(nd: NeoData): Desc = nd match {
    case NeoNode(_, labels, prop) if labels == Set("Artifact") => Artifact(prop.getOrElse("path", "???").asInstanceOf[String])
    case NeoNode(_, labels, prop) if labels == Set("Process") => Process(prop.getOrElse("name", "???").asInstanceOf[String]) //TODO: Option
    case NeoRel(_, t, _) => EdgeDesc(t)
  }

  @Ignore
  @Test
  def learnNeo4jOnlyActivities: Unit = {
    println("wajih")
    val driver = GraphDatabase.driver("bolt://localhost", AuthTokens.basic("neo4j", "oldnew"))
    val session = driver.session();
    val g = toDiGraph(run(session)("MATCH (n)-[r]-()  where n.type='Process' AND r.type='WasTriggeredBy' RETURN n,r;"))
    println(g)
    println("result")
    println(LearnDeterministicDag.greedyLearn(g, 10)(describe,describe_original))
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
    val driver = GraphDatabase.driver("bolt://localhost", AuthTokens.basic("neo4j", "oldnew"))
    println("wajihdone")
    val session = driver.session();
    
    println("!!!!")
    val g = fullGraph(session)
    // writeGraph1(g)
    println("result")
    // println(g)
    //    println(g.mkString(sep)
    println("hmmm")
    val dfa = LearnDeterministicDag.greedyLearn(g, 30)(describe, describe_original)
    // println(dfa)
    // val ggg = createGraphFromDag(dfa)
    // println("+++++++++++++++++")
    // println(ggg)
    // writeGraph2(ggg)
    // val ids=dfa.getpossibleIds(Process("ftpbench"))
    // dfa.getDescendants(ids.head._1)
  }
}
