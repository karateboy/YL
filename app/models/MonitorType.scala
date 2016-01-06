package models
import scala.collection.Map
import java.sql.Date
import play.api.Logger
import scalikejdbc._
import scalikejdbc.config._
import EnumUtils._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import models.ModelHelper._

case class MonitorType(id:String, desp:String, unit:String,
    std_law:Option[Float],
    itemId:String,
    prec:Int, order:Int)
    
object MonitorType extends Enumeration{
  implicit val mtReads: Reads[MonitorType.Value] = EnumUtils.enumReads(MonitorType)
  implicit val mtWrites: Writes[MonitorType.Value] = EnumUtils.enumWrites
  
  private def mtList:List[MonitorType] =
    DB readOnly{ implicit session =>
      sql"""
        Select *
        From MonitorType
      """.map { r =>  MonitorType(id = r.string(1), 
          desp = r.string(2),
          unit = r.string(3),
          std_law = r.floatOpt(4),
          itemId = r.string(5),
          prec = r.int(6),
          order = r.int(7)
          )}.list.apply
    }
  
  var map:Map[Value, MonitorType] = Map(mtList.map{e=>Value(e.id)->e}:_*)
  var itemIdMap = Map(mtList.map{e=>e.itemId->e}:_*)
  
  val mtvAllList = mtList.map(mt=>MonitorType.withName(mt.id))
  
  def newMonitorType(mt:MonitorType)={
    DB localTx { implicit session=>
      sql"""
        INSERT INTO [dbo].[MonitorType]
           ([ITEM]
           ,[DESP]
           ,[UNIT]
           ,[std_law]
           ,[ItemId]
           ,[precision]
           ,[order])
     VALUES
           (${mt.id}
           ,${mt.desp}
           ,${mt.unit}
           ,${mt.std_law}
           ,${mt.itemId}
           ,${mt.prec}
           ,${mt.order})        
        """.update.apply
    }
    map = map + (Value(mt.id)->mt)
    itemIdMap = itemIdMap + (mt.itemId -> mt)
  }
  
  val psiList = List()
  val windDirList = List(MonitorType.withName("WD_HR"))
    
  val epaMap={
    map.map(kv=>(kv._2.itemId, kv._1))
  }
  

  def format(mt: MonitorType.Value, v: Option[Float])={
    if(v.isEmpty)
      "-"
    else{
      val prec = map(mt).prec
      s"%.${prec}f".format(v.get)
    }
  }
}