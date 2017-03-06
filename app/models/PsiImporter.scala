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

object PsiImporter {
  def importData(path: String) = {
    val worker = Akka.system.actorOf(Props[PsiImporter], name = "psiImporter" + (Math.random() * 1000).toInt)
    worker ! ImportEpa(path)
  }

  import java.io.File
  import java.io.FileFilter
  def listAllFiles(dir: String) = {
    new java.io.File(dir).listFiles.filter(_.getName.endsWith(".xls"))
  }
}

class PsiImporter extends Actor {

  def receive = {
    case ImportEpa(path) =>
      val files = PsiImporter.listAllFiles(path)
      for (f <- files) {
        importPsiData(f)
        f.delete()
      }

      Logger.info("Finish import PSI")
      self ! PoisonPill
  }

  import java.io.File
  import java.io.FileInputStream
  def importPsiData(f: File) {
    Logger.debug(s"Import ${f.getAbsolutePath}")
    import scala.collection.mutable.ListBuffer
    val wb = WorkbookFactory.create(new FileInputStream(f));
    val sheet = wb.getSheetAt(0)

    var rowN = 1
    var finish = false
    val seqData = ListBuffer.empty[Seq[Any]]
    do {
      var row = sheet.getRow(rowN)
      if (row == null)
        finish = true
      else {
        try {
          val site = row.getCell(0).getStringCellValue
          val siteId =
            try {
              if (site == "台西")
                EpaMonitor.map(EpaMonitor.withName("臺西")).id
              else
                EpaMonitor.map(EpaMonitor.withName(site)).id
            }catch{
              case ex: NoSuchElementException=>
                val newID = EpaMonitor.newMonitorID
                EpaMonitor.newEpaMonitor(EpaMonitor(newID, site))
                newID    
            }

          Logger.debug(row.getCell(1).getStringCellValue)          
          val dateVal = try{
            row.getCell(1).getDateCellValue
          }catch{
            case _:Exception=>
              val str = row.getCell(1).getStringCellValue
              DateTime.parse(str, DateTimeFormat.forPattern("YYYY/MM/dd HH:mm")).toDate()
          }
            
          
          val v =
            for (col <- 2 to 7) yield {
              try {
                val v = row.getCell(col).getNumericCellValue
                Some(v.toInt)
              } catch {
                case _: Throwable =>
                  {
                    try {
                      val valStr = row.getCell(col).getStringCellValue
                      if (valStr.isEmpty())
                        None
                      else {
                        try {
                          Some(valStr.toInt)
                        } catch {
                          case _: Throwable =>
                            None
                        }
                      }
                    } catch {
                      case _: Throwable =>
                        None
                    }
                  }
              }
            }

          //val ts: java.sql.Timestamp = DateTime.parse(dateStr, DateTimeFormat.forPattern("YYYY/MM/dd"))
          seqData.append(Seq(siteId, dateVal, v(0), v(1), v(2), v(3), v(4), v(5)))
        } catch {
          case ex: Throwable =>
            Logger.error(ex.toString)
            Logger.error("Ignore this row")
            throw ex
        }

        rowN += 1
      }
    } while (!finish)

    wb.close()

    DB autoCommit { implicit session =>
      sql"""
        INSERT INTO [dbo].[PsiDay]
           ([Station]
           ,[Date]
           ,[CO]
           ,[O3]
           ,[NO2]
           ,[SO2]
           ,[PM10]
           ,[PSI])        
        values (
        ?,?,?,?,?,?,?,?)
        """
        .batch(seqData.toList: _*)
        .apply()
    }
  }
}