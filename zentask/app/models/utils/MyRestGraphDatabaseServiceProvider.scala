package models.utils
import org.neo4j.scala.RestGraphDatabaseServiceProvider
import java.net.URI
import org.neo4j.scala.Neo4jIndexProvider
import org.neo4j.scala.Neo4jBatchIndexProvider
import org.neo4j.kernel.impl.batchinsert.BatchInserter
import scala.collection.mutable.MutableList
import org.neo4j.graphdb.PropertyContainer
import org.neo4j.graphdb.index.Index
import org.neo4j.graphdb.Node

trait MyRestGraphDatabaseServiceProvider extends RestGraphDatabaseServiceProvider with Neo4jIndexProvider{
  
  private var nodeIndexList = MutableList[(String, IndexCustomConfig)]()
  private var relationIndexList = MutableList[(String, IndexCustomConfig)]()
  
  override def uri: URI = {
    new URI("localhost")
  }
  
  override def userPw: Option[(String, String)] ={
    Some("","")
  }
  
  override def NodeIndexConfig: List[(String, IndexCustomConfig)] = nodeIndexList.toList
    
  override def RelationIndexConfig: List[(String, IndexCustomConfig)] = relationIndexList.toList
  
  def addNodeIndex(indexName: String) = {
    nodeIndexList.+=((indexName, Some(Map("provider" -> "lucene", "type" -> "fulltext"))))
    getNodeIndex(indexName)
  }

  def addRelationshipIndex(indexName: String) = {
    relationIndexList.+=((indexName, Some(Map("provider" -> "lucene", "type" -> "fulltext"))))
    getRelationIndex(indexName)
  }

//    ("USER", Some(Map("provider" -> "lucene", "type" -> "fulltext"))) ::
//                                 ("TASK", Some(Map("provider" -> "lucene", "type" -> "fulltext"))) :: 
//                                 ("PROJECT", Some(Map("provider" -> "lucene", "type" -> "fulltext"))) :: 
//                                 ("ROLE", Some(Map("provider" -> "lucene", "type" -> "fulltext"))) :: 
//                                 Nil

}