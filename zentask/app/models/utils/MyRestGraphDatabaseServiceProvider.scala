package models.utils
import org.neo4j.scala.RestGraphDatabaseServiceProvider
import java.net.URI

trait MyRestGraphDatabaseServiceProvider extends RestGraphDatabaseServiceProvider{
  override def uri: URI = {
    new URI("localhost")
  }
  
  override def userPw: Option[(String, String)] ={
    Some("","")
  }
}