package models
import scalikejdbc._
import scalikejdbc.config._
import scala.collection.Map

/**
 * @author user
 */
case class EpaMonitor(id:Int, name:String)
object EpaMonitor extends Enumeration{    
  var monitorList = 
    DB readOnly{ implicit session =>
      sql"""
        Select * 
        From EpaMonitor
        """.map { r =>           
          EpaMonitor(r.int(1), r.string(2))}.list.apply
    }
  
  var map:Map[Value, EpaMonitor] = Map(monitorList.map{e=>Value(e.id, e.name)->e}:_*)
  var idMap:Map[Int, Value] = Map(monitorList.map{e=>e.id->EpaMonitor(e.id)}:_*)
  var mvList = monitorList.map{m=>EpaMonitor.withName(m.name)}

  def refresh = {
    monitorList =
      DB readOnly { implicit session =>
        sql"""
        Select * 
        From EpaMonitor
        """.map { r =>
          EpaMonitor(r.int(1), r.string(2))
        }.list.apply
      }
    map = Map(monitorList.map { e => Value(e.id, e.name) -> e }: _*)
    mvList = map.keys.toList
  }
  
  def newEpaMonitor(m : EpaMonitor) = {
    DB localTx { implicit session =>
      sql"""
        INSERT INTO [dbo].[EpaMonitor]
           ([SiteId]
           ,[Name])
     VALUES
           (${m.id},${m.name})
        """.update.apply
    }
    monitorList = (m::monitorList).sortBy { x => x.name }
    val v = Value(m.id, m.name)
    map = map + (v -> m)
    idMap = idMap + (m.id -> v)
    mvList = map.keys.toList
  }
  
  val YunlinMonitorList = List(37, 38, 41, 83).map { EpaMonitor.idMap }
}