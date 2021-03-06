package models
import scalikejdbc._
import scalikejdbc.config._
import scala.collection.Map

/**
 * @author user
 */
case class EpaMonitor(id:Int, name:String, districtID:Option[Int]=None, ordinary:Boolean=false,
    industrial:Boolean=false, traffic:Boolean=false, park:Boolean=false, background:Boolean=false, other:Boolean=false)
object EpaMonitor extends Enumeration{    
  var monitorList = 
    DB readOnly{ implicit session =>
      sql"""
        Select * 
        From EpaMonitor
        """.map { r =>           
          EpaMonitor(id=r.int(1), name=r.string(2), districtID=r.intOpt(3),
              ordinary=r.boolean(4),
              industrial=r.boolean(5),
              traffic=r.boolean(6),
              park=r.boolean(7),
              background=r.boolean(8),
              other=r.boolean(9)
          )}.list.apply
    }
  
  var map:Map[Value, EpaMonitor] = Map(monitorList.map{e=>Value(e.id, e.name)->e}:_*)
  var idMap:Map[Int, Value] = Map(monitorList.map{e=>e.id->EpaMonitor(e.id)}:_*)
  var mvList = monitorList.map{m=>EpaMonitor.withName(m.name)}

  def refresh = {
    monitorList =
    DB readOnly{ implicit session =>
      sql"""
        Select * 
        From EpaMonitor
        """.map { r =>           
          EpaMonitor(r.int(1), r.string(2), r.intOpt(3), r.boolean(4))}.list.apply
    }
    map = Map(monitorList.map { e => Value(e.id, e.name) -> e }: _*)
    mvList = map.keys.toList
  }
  
  def newMonitorID = {
    val maxId =idMap.keys.max
    if(maxId < 1000)
      1000
    else
      maxId +1
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
  def normalMonitor = mvList.filter(map(_).id < 1000)

  def getMonitorClassStr(m:EpaMonitor.Value)={
    EpaMonitorFilter.values.toList.filter { f=> EpaMonitorFilter.filter(f)(m) }.map { EpaMonitorFilter.map }.mkString(",")
  }
}