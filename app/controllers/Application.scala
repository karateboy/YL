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
        val sites = List("二林", "線西", "崙背", "斗六", "臺西", "麥寮", "竹山", "嘉義", "朴子")
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
                  val kh_data = data.filter { d => sites.contains(d.siteName) }.sortBy { d => sites.indexOf(d.siteName) }
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
    timeSet ++= getPeriods(DateTime.parse(start.toString("YYYY-MM-dd HH"), DateTimeFormat.forPattern("YYYY-MM-dd HH")),
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
        if (mtCase.std_law.isEmpty)
          None
        else
          Some(AxisLine("#FF0000", 2, mtCase.std_law.get, Some(AxisLineLabel("right", "法規值"))))

      val lines = Seq(std_law_line, None).filter { _.isDefined }.map { _.get }
      if (lines.length > 0)
        Some(lines)
      else
        None
    }

    val xAxis = {
      val duration = new Duration(start, end)
      if (duration.getStandardDays > 2)
        XAxis(None, gridLineWidth = Some(1), None)
      else
        XAxis(None)
    }

    val windMtCase = MonitorType.map(MonitorType.withName("C212"))
    val windYaxis = YAxis(None, AxisTitle(Some(Some(s"${windMtCase.desp} (${windMtCase.unit})"))), None,
      opposite = true,
      floor = Some(0),
      ceiling = Some(360),
      min = Some(0),
      max = Some(360),
      tickInterval = Some(45),
      gridLineWidth = Some(1),
      gridLineColor = Some("#00D800"))

    val chart =
      if (monitorTypes.length == 1) {
        val mt = monitorTypes(0)
        val mtCase = MonitorType.map(monitorTypes(0))

        HighchartData(
          Map("type" -> "line"),
          Map("text" -> title),
          xAxis,
          if (!monitorTypes.contains(windMtv))
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
                gridLineWidth = Some(0)),
                windYaxis)
            } else {
              Seq(YAxis(None, AxisTitle(Some(None)), None, gridLineWidth = Some(0)),
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

  def history = Security.Authenticated {
    implicit request =>
      Ok(views.html.history())
  }

  def historyReport(epaMonitorStr: String, monitorTypeStr: String,
                    startStr: String, endStr: String) = Security.Authenticated {
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

      var timeSet = Set[DateTime]()

      val epa_pairs =
        for {
          epa <- epaMonitors
          records = Record.getEpaHourRecord(epa, monitorTypes(0), start, end)
          timeRecords = records.map { t => t.time -> t.value }
          timeMap = Map(timeRecords: _*)
        } yield {
          timeSet ++= timeMap.keys
          epa -> timeMap
        }
      val epaRecordMap = Map(epa_pairs: _*)
      val title = "歷史資料查詢"
      val output = views.html.historyReport(epaMonitors, monitorTypes(0), start, end, timeSet.toList.sorted, epaRecordMap)
      Ok(output)
  }

  def windRose() = Security.Authenticated {
    implicit request =>
      Ok(views.html.windRose(false))
  }

  def monitorTypeRose() = Security.Authenticated {
    implicit request =>
      Ok(views.html.windRose(true))
  }

  def windRoseReport(monitorStr: String, monitorTypeStr: String, nWay: Int, startStr: String, endStr: String) = Security.Authenticated {
    val monitor = EpaMonitor.withName(monitorStr)
    val monitorType = MonitorType.withName(monitorTypeStr)
    val start = DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd HH:mm"))
    val end = DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd HH:mm"))
    val mtCase = MonitorType.map(monitorType)
    assert(nWay == 8 || nWay == 16 || nWay == 32)

    try {
      val level = List(1f, 2f, 5f, 15f)
      val windMap = Record.getMtRose(monitor, monitorType, start, end, level, nWay)
      val nRecord = windMap.values.map { _.length }.sum

      val dirMap =
        Map(
          (0 -> "北"), (1 -> "北北東"), (2 -> "東北"), (3 -> "東北東"), (4 -> "東"),
          (5 -> "東南東"), (6 -> "東南"), (7 -> "南南東"), (8 -> "南"),
          (9 -> "南南西"), (10 -> "西南"), (11 -> "西西南"), (12 -> "西"),
          (13 -> "西北西"), (14 -> "西北"), (15 -> "北北西"))

      val dirStrSeq =
        for {
          dir <- 0 to nWay - 1
          dirKey = if (nWay == 8)
            dir * 2
          else if (nWay == 32) {
            if (dir % 2 == 0) {
              dir / 2
            } else
              dir + 16
          } else
            dir
        } yield dirMap.getOrElse(dirKey, "")

      var last = 0f
      val speedLevel = level.flatMap { l =>
        if (l == level.head) {
          last = l
          List(s"< ${l} ${mtCase.unit}")
        } else if (l == level.last) {
          val ret = List(s"${last}~${l} ${mtCase.unit}", s"> ${l} ${mtCase.unit}")
          last = l
          ret
        } else {
          val ret = List(s"${last}~${l} ${mtCase.unit}")
          last = l
          ret
        }
      }

      import Highchart._

      val series = for {
        level <- 0 to level.length
      } yield {
        val data =
          for (dir <- 0 to nWay - 1)
            yield Seq(Some(dir.toDouble), Some(windMap(dir)(level).toDouble))

        seqData(speedLevel(level), data)
      }

      val title = 
        if(monitorType == MonitorType.C211)
            "風瑰圖"
        else{
          mtCase.desp + "玫瑰圖"
        }
      
      val chart = HighchartData(
        scala.collection.immutable.Map("polar" -> "true", "type" -> "column"),
        scala.collection.immutable.Map("text" -> title),
        XAxis(Some(dirStrSeq)),
        Seq(YAxis(None, AxisTitle(Some(Some(""))), None)),
        series)

        Results.Ok(Json.toJson(chart))
    } catch {
      case e: AssertionError =>
        Logger.error(e.toString())
        BadRequest("無資料")
    }
  }

  def pm10Query = Security.Authenticated {    
    Ok(views.html.OverLawQuery(MonitorType.A214))
  }
  
  def pm25Query = Security.Authenticated {    
    Ok(views.html.OverLawQuery(MonitorType.A215))
  }
  
  case class OverLawEntry(monitor:EpaMonitor.Value, monitorType:MonitorType.Value, date:DateTime, dayAvg:Float, max:Float, min:Float)
  def overLawReport(epaMonitorStr: String, monitorTypeStr: String,
                    startStr: String, endStr: String) = Security.Authenticated {
    implicit request =>
      import scala.collection.JavaConverters._
      val epaMonitors = epaMonitorStr.split(':').map { EpaMonitor.withName }
      val monitorType = MonitorType.withName(monitorTypeStr)
      val (start, end) =
        (DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd")),
          DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd")))

      val mtCase = MonitorType.map(monitorType)
      var timeSet = Set[DateTime]()

      def days()={
        import scala.collection.mutable.ListBuffer
        val buf = ListBuffer.empty[DateTime]
        var current = start
        while(current<end){
          buf.append(current)
          current += 1.day
        }  
        buf.toList
      }
      
      val overLawRecords =
        for {
          epa <- epaMonitors
          records = Record.getEpaHourRecord(epa, monitorType, start, end)
        } yield {
          for{current <- days()
            day_records = records.filter { r => (r.time >=current && r.time < current + 1.day) }.map { _.value }
            count = day_records.length 
            dayAvg = day_records.sum / count if mtCase.std_law.isDefined && dayAvg > mtCase.std_law.get
          }
            yield OverLawEntry(epa, monitorType, current, dayAvg, day_records.max, day_records.min)
        }
        
       Ok(views.html.OverLawReport(monitorType, start, end, overLawRecords))
  }
  
  def siteInfo  = Security.Authenticated {
    Ok(views.html.siteInfo())
  }
}
