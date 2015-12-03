package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Json
import play.api.Play.current
import play.api.data._
import play.api.data.Forms._
import play.api.libs.ws._
import play.api.libs.ws.ning.NingAsyncHttpClientConfigBuilder
import scala.concurrent.Future
import play.api.libs.json._
import com.github.nscala_time.time.Imports._
import Highchart._
import models._

object Application extends Controller {

  import play.api.libs.concurrent.Execution.Implicits.defaultContext
  case class EpaRealtimeData(
    siteName: String,
    county: String,
    psi: String,
    so2: String,
    co: String,
    o3: String,
    pm10: String,
    pm25: String,
    no2: String,
    windSpeed: String,
    windDir: String,
    publishTime: String)

  implicit val epaRealtimeDataRead: Reads[EpaRealtimeData] =
    ((__ \ "SiteName").read[String] and
      (__ \ "County").read[String] and
      (__ \ "PSI").read[String] and
      (__ \ "SO2").read[String] and
      (__ \ "CO").read[String] and
      (__ \ "O3").read[String] and
      (__ \ "PM10").read[String] and
      (__ \ "PM2.5").read[String] and
      (__ \ "NO2").read[String] and
      (__ \ "WindSpeed").read[String] and
      (__ \ "WindDirec").read[String] and
      (__ \ "PublishTime").read[String])(EpaRealtimeData.apply _)

      
  def index = Action.async {
    implicit request =>
      {
        val url = "http://opendata.epa.gov.tw/ws/Data/AQX/?$orderby=SiteName&$skip=0&$top=1000&format=json"
        WS.url(url).get().map {
          response =>
            try {
              val epaData = response.json.validate[Seq[EpaRealtimeData]]
              epaData.fold(
                error => {
                  Logger.error(JsError.toJson(error).toString())
                  Ok(views.html.realtime(Seq.empty[EpaRealtimeData]))
                },
                data => {
                  val kh_data = data.filter { d => d.county == "雲林縣" || d.county == "彰化縣" || d.county == "嘉義縣" }
                  Ok(views.html.realtime(kh_data))
                })
            } catch {
              case ex: Exception =>
                Logger.error(ex.toString())
                Ok(views.html.realtime(Seq.empty[EpaRealtimeData]))
            }
        }
      }
  }

  def historyTrend = Security.Authenticated {
    implicit request =>
      Ok(views.html.historyTrend())
  }

    def trendHelper(epaMonitors: Array[EpaMonitor.Value],
                  monitorTypes: Array[MonitorType.Value], start: DateTime, end: DateTime) = {

        def getPeriods(start: DateTime, endTime: DateTime, d: Period): List[DateTime] = {
    import scala.collection.mutable.ListBuffer

    val buf = ListBuffer[DateTime]()
    var current = start
    while (current < endTime) {
      buf.append(current)
      current += d
    }

    buf.toList
  }
    val windMtv = MonitorType.withName("C212")
    var timeSet = Set.empty[DateTime]
    timeSet ++=getPeriods(DateTime.parse(start.toString("YYYY-MM-dd HH"), DateTimeFormat.forPattern("YYYY-MM-dd HH")), 
                DateTime.parse(end.toString("YYYY-MM-dd HH"), DateTimeFormat.forPattern("YYYY-MM-dd HH")), 1.hour)
    val timeSeq = timeSet.toList.sorted.zipWithIndex
    
    def epaSeries() = {
      val epaMonitorPairs =
        for {
          m <- epaMonitors
        } yield {
          val epaMtPairs =
            for {
              mt <- monitorTypes
              epaRecord = Record.getEpaHourRecord(m, mt, start, end)
              epaPairs = epaRecord.map { r => r.time -> r.value }
              epaMap = Map(epaPairs: _*)
            } yield {
              mt -> epaMap
            }
          m -> Map(epaMtPairs: _*)
        }
        
      val epaRecordMap = Map(epaMonitorPairs: _*)
      for {
        m <- epaMonitors
        mt <- monitorTypes
        timeData = timeSeq.map { t =>
          val time = t._1
          val x = t._2
          if (epaRecordMap(m)(mt).contains(time))
            Seq(Some(time.getMillis.toDouble), Some(epaRecordMap(m)(mt)(time).toDouble))
          else
            Seq(Some(time.getMillis.toDouble), None)
        }
      } yield {
        if (monitorTypes.length > 1 && monitorTypes.contains(windMtv)) {
          if (mt != windMtv)
            seqData(EpaMonitor.map(m).name + "_" + MonitorType.map(mt).desp, timeData)
          else
            seqData(EpaMonitor.map(m).name + "_" + MonitorType.map(mt).desp, timeData, 1, Some("scatter"))
        } else {
          seqData(EpaMonitor.map(m).name + "_" + MonitorType.map(mt).desp, timeData)
        }
      }
    }

    val epa_series = epaSeries()

    val series = epa_series

    
    val downloadFileName = {
      val startName = start.toString("YYMMdd")
      val mNames = epaMonitors.map { EpaMonitor.map(_).name }
      val mtNames = monitorTypes.map { MonitorType.map(_).desp }
      startName + mNames.mkString + mtNames.mkString
    }
    
    val title = "趨勢圖"

    def getAxisLines(mt: MonitorType.Value) = {
      val mtCase = MonitorType.map(mt)
      val std_law_line =
        if(mtCase.std_law.isEmpty)
          None
        else
          Some(AxisLine("#FF0000", 2, mtCase.std_law.get, Some(AxisLineLabel("right", "法規值"))))
            
      val lines = Seq(std_law_line, None).filter { _.isDefined }.map { _.get }
      if(lines.length > 0)
        Some(lines)
      else
        None
    }

    val xAxis = {
          val duration = new Duration(start, end)
          if(duration.getStandardDays > 2)
            XAxis(None, gridLineWidth=Some(1), None)
          else
            XAxis(None)
       }
    
    val windMtCase = MonitorType.map(MonitorType.withName("C212"))
    val windYaxis =  YAxis(None, AxisTitle(Some(Some(s"${windMtCase.desp} (${windMtCase.unit})"))), None, 
                    opposite=true, 
                    floor=Some(0), 
                    ceiling=Some(360),
                    min=Some(0),
                    max=Some(360),
                    tickInterval=Some(45), 
                    gridLineWidth=Some(1),
                    gridLineColor=Some("#00D800"))
                    
    val chart =
      if (monitorTypes.length == 1) {
        val mt = monitorTypes(0)
        val mtCase = MonitorType.map(monitorTypes(0))
        
        HighchartData(
          Map("type" -> "line"),
          Map("text" -> title),
          xAxis,
          if(!monitorTypes.contains(windMtv))
            Seq(YAxis(None, AxisTitle(Some(Some(s"${mtCase.desp} (${mtCase.unit})"))), getAxisLines(mt)))
          else
            Seq(windYaxis),
          series,
          Some(downloadFileName))
      } else {
        val yAxis =
          if (monitorTypes.contains(windMtv)) {
            if (monitorTypes.length == 2) {
              val mt = monitorTypes.filter { !MonitorType.windDirList.contains(_) }(0)
              val mtCase = MonitorType.map(monitorTypes.filter { !MonitorType.windDirList.contains(_) }(0))
              Seq(YAxis(None, 
                  AxisTitle(Some(Some(s"${mtCase.desp} (${mtCase.unit})"))), 
                  getAxisLines(mt),
                  gridLineWidth=Some(0)),
                  windYaxis)
            } else {              
              Seq(YAxis(None, AxisTitle(Some(None)), None, gridLineWidth=Some(0)), 
                  windYaxis)
            }
          } else {
              Seq(YAxis(None, AxisTitle(Some(None)), None))
          }

        HighchartData(
          Map("type" -> "line"),
          Map("text" -> title),
          xAxis,
          yAxis,
          series,
          Some(downloadFileName))
      }

    chart
  }

  def historyTrendChart(epaMonitorStr: String, monitorTypeStr: String,
                        startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      import scala.collection.JavaConverters._
      val epaMonitors = if (epaMonitorStr.equalsIgnoreCase("None"))
        Array.empty[EpaMonitor.Value]
      else
        epaMonitorStr.split(':').map { EpaMonitor.withName }
      val monitorTypeStrArray = monitorTypeStr.split(':')
      val monitorTypes = monitorTypeStrArray.map { MonitorType.withName }
      val (start, end) =
          (DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd HH:mm")),
            DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd HH:mm")))
            
      val outputType = OutputType.withName(outputTypeStr)

      val chart = trendHelper(epaMonitors, monitorTypes, start, end)

      Results.Ok(Json.toJson(chart))
  }

}
