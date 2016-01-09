package models
import scalikejdbc._
import scalikejdbc.config._
import scala.collection.Map

case class District(id:Int, name:String)
object District extends Enumeration{
  val list = 
    DB readOnly{ implicit session =>
      sql"""
        Select * 
        From District
        """.map { r =>           
          District(r.int(1), r.string(2))}.list.apply
    }
  
  val map:Map[Value, District] = Map(list.map{e=>Value(e.id)->e}:_*)
  val idMap:Map[Int, Value] = Map(list.map{e=>e.id->District(e.id)}:_*)
  val mvList = list.map{d=>District(d.id)}
  def getEpaMonitorNameStr(d:District.Value)={
    val monitorNames = EpaMonitor.mvList.filter { EpaMonitor.map(_).districtID == Some(d.id) }.map { EpaMonitor.map(_).name }.mkString(", ")
    monitorNames
  }
  
  def getNormalEpaMonitorNameStr(d:District.Value)={
    val monitorNames = EpaMonitor.normalMonitor.filter { EpaMonitor.map(_).districtID == Some(d.id) }.map { EpaMonitor.map(_).name }.mkString(", ")
    monitorNames
  }
}