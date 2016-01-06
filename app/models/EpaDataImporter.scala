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
case class FinishImport(offset: Int, count: Int, total: Int)

class EpaDataImporter extends Actor {
  var errorCount = 0
  def receive = {
    case ImportYesterday =>
      importEpaDataTask(29000, 1000, self)

    case FinishImport(offset, count, total) =>
      if (total == count) {
        //more to import
        Logger.debug(s"offset=$offset total=$total count=$count.")
        importEpaDataTask(offset + count, count, self)
      } else {
        Logger.debug(s"total=$total count=$count. Consider import done")

        //no more and end itself
        self ! PoisonPill
      }
  }

  def importEpaDataTask(offset: Int, count: Int, importer: ActorRef) {
    val result = EpaHourData.getEpaData(offset, count).map { hrDataSeq =>
      import scala.collection.mutable.ListBuffer
      
      val nData = hrDataSeq.length
      Logger.debug("recv #="+nData)
      val seqData = ListBuffer.empty[Seq[Any]]

      for (hr <- hrDataSeq) {
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

      Logger.debug("data ready #=" + seqData.length)
      DB localTx { implicit session =>
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
      importer ! FinishImport(offset, count, nData)
    }

    def retryImport={
        errorCount += 1
        if (errorCount <= 10) {
          Logger.debug(s"try again errorCount=$errorCount")
          importEpaDataTask(offset, count, importer)
        } else {
          Logger.debug("give up import task.")
          importer ! FinishImport(offset, count, 0)
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
        Logger.info("Take it as success...")
        importer ! FinishImport(offset, count, count)
      case ex: Exception=>
        Logger.error(ex.toString())
        retryImport
    }
  }
}
