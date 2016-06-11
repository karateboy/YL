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
import org.apache.poi.openxml4j.opc._
import org.apache.poi.xssf.usermodel._
import org.apache.poi.ss.usermodel._

object Epa100Importer {
  def importData(path: String) = {
    val worker = Akka.system.actorOf(Props[Epa100Importer], name = "epaImporter" + (Math.random() * 1000).toInt)
    worker ! ImportEpa(path)
  }
  case class IncCount(name:String)
  case class DecCount(name:String)
}

class Epa100Importer extends Actor {
  import java.io.File
  import java.io.FileFilter
  import Epa100Importer._
  
  def listAllFiles(dir: String) = {
    new java.io.File(dir).listFiles.filter(_.getName.endsWith(".csv"))
  }
  import scala.concurrent._
  var nFile = 0
  def receive = {
    case ImportEpa(path) =>
      val files = listAllFiles(path)
      for (f <- files) {
        Future{
          blocking{
            importEpaData(f)
            f.delete()            
          }
        } onFailure({
          case ex:Throwable=>
            Logger.error(s"Failed to import ${f.getAbsolutePath}", ex)
        })
      }
    case IncCount(name)=>
      nFile+=1
      Logger.info(s"Concurrent $nFile: $name")
    
    case DecCount(name)=>
      nFile-=1
      Logger.info(s"Concurrent $nFile: $name finished")
      
      if(nFile == 0)
        Logger.info("Finish import!")
      
  }

  import java.io.File
  import scala.io.Source
  def importEpaData(f: File) {
    self ! IncCount(s"Import ${f.getAbsolutePath}")
    import scala.collection.mutable.ListBuffer
    val seqData = ListBuffer.empty[Seq[Any]]

    def processLine(line: String){
      val parts = line.split(",").map { _.replace("\"", "") }.map(_.trim)
      val dateStr = parts(0)
      val site = parts(1).replace("台", "臺")

      val siteId = EpaMonitor.map(EpaMonitor.withName(site)).id

      val mt = parts(2)
      val itemId = MonitorType.map(MonitorType.withName(mt)).itemId
      val values =
        for (col <- 3 to 3 + 23) yield {
          try {
            val v = parts(col)
            if (v.isEmpty() || v.contains("#") || v.contains("*") || v.contains("x"))
              None
            if (v.equalsIgnoreCase("NR"))
              Some(0f)
            else
              Some(v.toFloat)
          } catch {
            case _: Throwable =>
              None
          }
        }
      def appendHr(value: Option[Float], offset: Int) = {
        val ts: java.sql.Timestamp = DateTime.parse(dateStr, DateTimeFormat.forPattern("YYYY/MM/dd")) + offset.hour
        seqData.append(Seq(siteId, ts, itemId, value))
      }

      for (v <- values.zipWithIndex)
        appendHr(v._1, v._2)
    }

    def readCsv() = {
      for (line <- Source.fromFile(f).getLines().zipWithIndex) {
        try {
          //skip first line
          if(line._2 != 0)
            processLine(line._1)
        } catch {
          case ex: Throwable =>
            Logger.error(ex.getMessage)
            Logger.error("Ignore this line")
        }
      }
    }

    readCsv

    DB autoCommit { implicit session =>
      sql"""
        INSERT INTO [dbo].[hour_data_temp]
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
    
    self ! DecCount(s"Import ${f.getAbsolutePath}")
  }
}