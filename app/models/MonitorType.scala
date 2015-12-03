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
    std_internal_default:Option[Float], std_law:Option[Float], std_hour:Option[Float],
    std_day:Option[Float], std_year:Option[Float], 
    zd_internal:Option[Float], zd_law:Option[Float],
    sd_internal:Option[Float], sd_law:Option[Float],
    epa_mapping:Option[String],
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
          std_internal_default = r.floatOpt(5),
          std_law = r.floatOpt(6), 
          std_hour = r.floatOpt(7),
          std_day = r.floatOpt(8), 
          std_year = r.floatOpt(9),
          zd_internal = r.floatOpt(10),
          zd_law = r.floatOpt(11),
          sd_internal = r.floatOpt(12),
          sd_law = r.floatOpt(13),
          epa_mapping = r.stringOpt(14),
          prec = r.int(15),
          order = r.int(16)
          )}.list.apply
    }
  
  var map:Map[Value, MonitorType] = Map(mtList.map{e=>Value(e.id)->e}:_*) - MonitorType.withName("A325")
  val mtvAllList = mtList.map(mt=>MonitorType.withName(mt.id)).filter { !List(MonitorType.withName("A325"), MonitorType.withName("C911"), MonitorType.withName("C912")).contains(_) }
  
  def updateMonitorType(mt: MonitorType.Value, colname: String, newValue: String) = {
    DB localTx { implicit session =>
      val updateValue =
        if (newValue == "-")
          None
        else {
          import java.lang.Float
          val v = Float.parseFloat(newValue)
          Some(v)
        }

      val col = SQLSyntax.createUnsafely(s"${colname}")
      sql"""
        Update MonitorType
        Set ${col}=${updateValue}
        Where ITEM=${mt.toString}  
        """.update.apply

      val old = map(mt)

      val newMtOpt =
        sql"""
          Select *
          From MonitorType
          Where ITEM=${mt.toString}
        """.map { r =>
          MonitorType(id = r.string(1),
            desp = r.string(2),
            unit = r.string(3),
            std_internal_default = r.floatOpt(5),
            std_law = r.floatOpt(6),
            std_hour = r.floatOpt(7),
            std_day = r.floatOpt(8),
            std_year = r.floatOpt(9),
            zd_internal = r.floatOpt(10),
            zd_law = r.floatOpt(11),
            sd_internal = r.floatOpt(12),
            sd_law = r.floatOpt(13),
            epa_mapping = r.stringOpt(14),
            prec = r.int(15),
            order = r.int(16))
        }.single.apply
      map = (map + (mt -> newMtOpt.get))
    }
  }
  
  val psiList = List(MonitorType.withName("A214"),MonitorType.withName("A222"), MonitorType.withName("A224"), MonitorType.withName("A225"), MonitorType.withName("A293") )
  val windDirList = List(MonitorType.withName("C212"), MonitorType.withName("C912"))
    
  val epaList = {
    val name=List("A214", "A215", "A222", "A223", "A224", "A225", "A226", "A283", "A286", "A293", "A296", "C211", "C212", "C213", "C214", "C215")
    name.map { MonitorType.withName }
  }
  
  val epaReportList ={
    val name=List("C212", "C211", "A222", "A293", "A224", "A225", "A214", "A226", "A296")
    name.map { MonitorType.withName }
  }
  
  val epaMap={
    map.filter(p=>p._2.epa_mapping.isDefined).map(kv=>(kv._2.epa_mapping.get, kv._1))
  }
  
  val A213 = MonitorType.withName("A213")
  val A214 = MonitorType.withName("A214")
  val A215 = MonitorType.withName("A215")
  val A221 = MonitorType.withName("A221")
  val A222 = MonitorType.withName("A222")
  val A223 = MonitorType.withName("A223")
  val A224 = MonitorType.withName("A224")
  val A225 = MonitorType.withName("A225")
  val A226 = MonitorType.withName("A226")
  val A229 = MonitorType.withName("A229")
  val A232 = MonitorType.withName("A232")
  val A233 = MonitorType.withName("A233")
  val A235 = MonitorType.withName("A235")
  val A283 = MonitorType.withName("A283")
  val A286 = MonitorType.withName("A286")
  val A288 = MonitorType.withName("A288")
  val A289 = MonitorType.withName("A289")
  val A293 = MonitorType.withName("A293")
  val A296 = MonitorType.withName("A296")
  val C211 = MonitorType.withName("C211")
  val C212 = MonitorType.withName("C212")
  val C213 = MonitorType.withName("C213")
  val C214 = MonitorType.withName("C214")
  val C215 = MonitorType.withName("C215")
  val C216 = MonitorType.withName("C216")

  def format(mt: MonitorType.Value, v: Option[Float])={
    if(v.isEmpty)
      "-"
    else{
      val prec = map(mt).prec
      s"%.${prec}f".format(v.get)
    }
  }
}