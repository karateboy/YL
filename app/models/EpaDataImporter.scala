package models
import play.api._
import akka.actor._
import com.github.nscala_time.time.Imports._
import play.api.Play.current
import play.api.libs.concurrent.Akka
import ModelHelper._
import play.api.libs.ws._
import play.api.libs.ws.ning.NingAsyncHttpClientConfigBuilder
import scala.concurrent.ExecutionContext.Implicits.global
import scalikejdbc._

object EpaDataImporter {
  def importYesterday={
    val worker = Akka.system.actorOf(Props[EpaDataImporter], name = "epaImporter" + (Math.random()*1000).toInt)
    worker ! ImportYesterday
  }
}

object ImportYesterday
case class FinishImport(offset: Int, count: Int, total: Int, date:DateTime)

class EpaDataImporter extends Actor {
  var errorCount = 0
  def receive = {
    case ImportYesterday =>
      val yesterday = DateTime.yesterday().toLocalDate().toDateTimeAtStartOfDay()
      val alreadyInDb = getRecordCountOnDate(yesterday)
      Logger.debug(s"Already ${alreadyInDb} in DB for ${yesterday}")
      if(alreadyInDb.isDefined)
        importEpaDataTask(alreadyInDb.get/24, 1000, yesterday, self)
      else
        importEpaDataTask(0, 1000, yesterday, self)
        
    case FinishImport(offset, count, total, date) =>
      if (total == count) {
        //more to import
        Logger.debug(s"offset=$offset total=$total count=$count.")
        importEpaDataTask(offset + count, count, date, self)
      } else {
        Logger.debug(s"total=$total count=$count. Consider import done")

        //no more and end itself
        self ! PoisonPill
      }
  }

  def getRecordCountOnDate(date: DateTime) = {
    val ts: java.sql.Timestamp = date
    DB readOnly { implicit session =>
      sql"""
      Select count(*)
      From hour_data
      Where MDate >= ${ts}
      """.map { _.int(1) }.single.apply
    }
  }

  def importEpaDataTask(offset:Int, count:Int, date:DateTime, importer: ActorRef) {
    Logger.debug(s"Import EPA data later than $date")
    val result = EpaHourData.getEpaData(offset, count).map { hrDataSeq =>
      import scala.collection.mutable.ListBuffer
      
      val newData = hrDataSeq.filter { _.MonitorDate.toDateTime >=date }
      
      val seqData = ListBuffer.empty[Seq[Any]]

      for (hr <- newData) {
        def appendHr(hrData: Option[String], offset: Int) {
          import scala.collection.mutable.ArrayBuffer
          val params = ArrayBuffer.empty[Any]

          val siteId = hr.SiteId.toInt
          params.append(siteId)
          if(EpaMonitor.idMap.get(siteId).isEmpty){
            EpaMonitor.newEpaMonitor(EpaMonitor(siteId, hr.SiteName))
          }
          val ts:java.sql.Timestamp = hr.MonitorDate.toDateTime + offset.hour
          params.append(ts)
          
          val itemId = hr.ItemId
          if(MonitorType.itemIdMap.get(itemId).isEmpty){
            val newMt = MonitorType(id=hr.ItemEngName, 
                desp=hr.ItemName, 
                unit=hr.ItemUnit,
                std_law=None,
                itemId=hr.ItemId,
                prec=1, 
                order=MonitorType.itemIdMap.size+1)
            
            MonitorType.newMonitorType(newMt)
          }
          params.append(itemId)
          if (hrData.isEmpty) {
            Logger.debug("Null value")
            params.append(None:Option[Float])
          } else {
            try {
              val value = hrData.get.toFloat
              params.append(value)
            } catch {
              case ex: Exception =>
                params.append(None:Option[Float])
            }
          }
          seqData.append(params.toSeq)
        }

        appendHr(hr.MonitorValue00, 0)
        appendHr(hr.MonitorValue01, 1)
        appendHr(hr.MonitorValue02, 2)
        appendHr(hr.MonitorValue03, 3)
        appendHr(hr.MonitorValue04, 4)
        appendHr(hr.MonitorValue05, 5)
        appendHr(hr.MonitorValue06, 6)
        appendHr(hr.MonitorValue07, 7)
        appendHr(hr.MonitorValue08, 8)
        appendHr(hr.MonitorValue09, 9)
        appendHr(hr.MonitorValue10, 10)
        appendHr(hr.MonitorValue11, 11)
        appendHr(hr.MonitorValue12, 12)
        appendHr(hr.MonitorValue13, 13)
        appendHr(hr.MonitorValue14, 14)
        appendHr(hr.MonitorValue15, 15)
        appendHr(hr.MonitorValue16, 16)
        appendHr(hr.MonitorValue17, 17)
        appendHr(hr.MonitorValue18, 18)
        appendHr(hr.MonitorValue19, 19)
        appendHr(hr.MonitorValue20, 20)
        appendHr(hr.MonitorValue21, 21)
        appendHr(hr.MonitorValue22, 22)
        appendHr(hr.MonitorValue23, 23)
      }

      DB autoCommit { implicit session =>
        sql"""
        INSERT INTO [dbo].[hour_data]
        ([MStation]
           ,[MDate]
           ,[MItem]
           ,[MValue])
        values (
        ?,?,?,?)
        """
          .batch(seqData.toList: _*)
          .apply()
      }
      errorCount=0
      importer ! FinishImport(offset, count, newData.length, date)
    }

    def retryImport={
        errorCount += 1
        if (errorCount <= 10) {
          Logger.debug(s"try again errorCount=$errorCount")
          importEpaDataTask(offset, count, date, importer)
        } else {
          Logger.debug("give up import task.")
          importer ! FinishImport(offset, count, 0, date)
        }
    }
    
    result.onFailure {
      case ex: java.util.concurrent.TimeoutException=>
        Logger.error(ex.toString())
        retryImport
      case ex: com.fasterxml.jackson.core.JsonParseException =>
        Logger.error(ex.toString())
        retryImport
      case ex: java.sql.BatchUpdateException=>
        Logger.error(ex.toString())
        Logger.info("Hit overlap. Take it as success...")
        importer ! FinishImport(offset, count, 0, date)
      case ex: Exception=>
        Logger.error(ex.toString())
        retryImport
    }
  }
}
