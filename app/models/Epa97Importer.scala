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

object Epa97Importer {
  def importData(path: String) = {
    val worker = Akka.system.actorOf(Props[Epa97Importer], name = "epaImporter" + (Math.random() * 1000).toInt)
    worker ! ImportEpa(path)
  }

  import java.io.File
  import java.io.FileFilter
  def listAllFiles(dir: String) = {
    new java.io.File(dir).listFiles.filter(_.getName.endsWith(".xls"))
  }
}

class Epa97Importer extends Actor {
  val concurrentFile = 25
  import java.io.File
  import java.io.FileInputStream
  import scala.concurrent._
  import Epa103Importer._

  def receive = handler(List.empty[File], 0)

  def importFileFuture(f: File) = {
    val future = Future {
      blocking {
        importEpaData(f)
        f.delete()
      }
    }

    future onFailure ({
      case ex: Throwable =>
        Logger.error(s"Failed to import ${f.getAbsolutePath}", ex)
    })

    future.onComplete { _ => self ! DecCount(f.getAbsolutePath) }
    f
  }
  def handler(pendingList: List[File], nFile: Int): Receive = {
    case ImportEpa(path) =>
      val files = listAllFiles(path)
      val fileList = files.toList
      val (processing, rest) = (fileList.take(concurrentFile), fileList.drop(concurrentFile))

      for (f <- processing)
        importFileFuture(f)

      context become handler(rest, processing.length)

    case StartImport(name) =>
      Logger.info(s"Concurrent $nFile: $name")

    case DecCount(name) =>
      Logger.info(s"Concurrent $nFile: $name finished")
      if (pendingList.isEmpty) {
        context become handler(List.empty[File], nFile - 1)
        if ((nFile - 1) == 0)
          Logger.info("Finish import!")
      } else {
        context become handler(pendingList.tail, nFile)
        importFileFuture(pendingList.head)
      }
  }

  def importEpaData(f: File) {
    self ! StartImport(s"Import ${f.getAbsolutePath}")
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
          val date = row.getCell(0).getDateCellValue
          val site = row.getCell(1).getStringCellValue
          val siteId = {
            val epaVal = try {
              EpaMonitor.withName(site)
            } catch {
              case ex: Throwable =>
                val replaceSite = site.replace("台", "臺")
                EpaMonitor.withName(replaceSite)
            }

            EpaMonitor.map(epaVal).id
          }
          
          val mt = row.getCell(2).getStringCellValue
          val itemId = MonitorType.map(MonitorType.withName(mt)).itemId
          val values =
            for (col <- 3 to 3 + 23) yield {
              try {
                val v = row.getCell(col).getNumericCellValue
                Some(v.toFloat)
              } catch {
                case _: Throwable =>
                  {
                    try {
                      val valStr = row.getCell(col).getStringCellValue
                      if (valStr.isEmpty())
                        None
                      else if (valStr.equalsIgnoreCase("NR"))
                        Some(0f)
                      else {
                        try {
                          Some(valStr.toFloat)
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

          def appendHr(value: Option[Float], offset: Int) = {
            val ts: java.sql.Timestamp = new DateTime(date) + offset.hour
            seqData.append(Seq(siteId, ts, itemId, value))
          }

          for (v <- values.zipWithIndex)
            appendHr(v._1, v._2)

        } catch {
          case ex: Throwable =>
            Logger.error(ex.getMessage)
            Logger.error("Ignore this row")
        }

        rowN += 1
      }
    } while (!finish)

    wb.close()

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
  }
}