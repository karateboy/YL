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

object Query extends Controller {
  def historyTrend = Security.Authenticated {
    implicit request =>
      Ok(views.html.historyTrend())
  }

  def windAvg(sum_sin: Double, sum_cos: Double) = {
    val degree = Math.toDegrees(Math.atan2(sum_sin, sum_cos)).toFloat
    if (degree >= 0)
      degree
    else
      degree + 360
  }

  def windAvgF(windSpeed: List[Float], windDir: List[Float]): Float = {
    if (windSpeed.length != windDir.length)
      Logger.error(s"windSpeed #=${windSpeed.length} windDir #=${windDir.length}")

    val windRecord = windSpeed.zip(windDir)
    val wind_sin = windRecord.map(v => v._1 * Math.sin(Math.toRadians(v._2))).sum
    val wind_cos = windRecord.map(v => v._1 * Math.cos(Math.toRadians(v._2))).sum
    windAvg(wind_sin, wind_cos)
  }

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

  def getPeriodReportMap(monitor: EpaMonitor.Value, mt: MonitorType.Value, period: Period)(start: DateTime, end: DateTime) = {
    val epaRecordList = Record.getEpaHourRecord(monitor, mt, start, end)
    def periodSlice(period_start: DateTime, period_end: DateTime) = {
      epaRecordList.dropWhile { _.time < period_start }.takeWhile { _.time < period_end }
    }
    val pairs =
      if (period.getHours ==1) {
        epaRecordList.filter(_.value.isDefined).map { r => r.time -> r.value.get }
      } else {
        for {
          period_start <- getPeriods(start, end, period)
          epaRecord = periodSlice(period_start, period_start + period)
          values = epaRecord.filter { r => r.value.isDefined }.map { r => r.value.get } if values.length > 0
        } yield {
          if (mt == MonitorType.withName("WD_HR")) {
            val windDir = values
            val windSpeed = Record.getEpaHourRecord(monitor, MonitorType.withName("WS_HR"), period_start, period_start + period).filter { r => r.value.isDefined }.map { r => r.value.get }
            period_start -> windAvgF(windSpeed, windDir)
          } else
            period_start -> values.sum / values.length

        }
      }

    Map(pairs: _*)
  }

  def trendHelper(epaMonitors: Array[EpaMonitor.Value],
                  monitorTypes: Array[MonitorType.Value], reportUnit: ReportUnit.Value, start: DateTime, end: DateTime) = {

    val windMtv = MonitorType.withName("WD_HR")
    val period: Period =
      reportUnit match {
        case ReportUnit.Hour =>
          1.hour
        case ReportUnit.Day =>
          1.day
        case ReportUnit.Month =>
          1.month
        case ReportUnit.Quarter =>
          3.month
        case ReportUnit.Year =>
          1.year
      }

    val timeSet = getPeriods(start, end, period)
    val timeSeq = timeSet.toList.sorted.zipWithIndex

    def epaSeries() = {

      val epaMonitorPairs =
        for {
          m <- epaMonitors
        } yield {
          val epaMtPairs =
            for {
              mt <- monitorTypes
              epaMap = getPeriodReportMap(m, mt, period)(start, end)
            } yield {
              mt -> epaMap
            }
          m -> Map(epaMtPairs: _*)
        }

      val epaRecordMap = Map(epaMonitorPairs: _*)
      for {
        m <- epaMonitors
        mt <- monitorTypes
        valueMap = epaRecordMap(m)(mt)
        timeData = timeSeq.map { t =>
          val time = t._1
          val x = t._2
          if (valueMap.contains(time))
            Seq(Some(time.getMillis.toDouble), Some(valueMap(time).toDouble))
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

    val series = epaSeries()

    val downloadFileName = {
      val startName = start.toString("YYMMdd")
      val mNames = epaMonitors.map { EpaMonitor.map(_).name }
      val mtNames = monitorTypes.map { MonitorType.map(_).desp }
      startName + mNames.mkString + mtNames.mkString
    }

    val title =
      reportUnit match {
        case ReportUnit.Hour =>
          s"趨勢圖 (${start.toString("YYYY年MM月dd日 HH:mm")}~${end.toString("YYYY年MM月dd日 HH:mm")})"
        case ReportUnit.Day =>
          s"趨勢圖 (${start.toString("YYYY年MM月dd日")}~${end.toString("YYYY年MM月dd日")})"
        case ReportUnit.Month =>
          s"趨勢圖 (${start.toString("YYYY年MM月")}~${end.toString("YYYY年MM月dd日")})"
        case ReportUnit.Quarter =>
          s"趨勢圖 (${start.toString("YYYY年MM月")}~${end.toString("YYYY年MM月dd日")})"
        case ReportUnit.Year =>
          s"趨勢圖 (${start.toString("YYYY年")}~${end.toString("YYYY年")})"
      }

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

    val windMtCase = MonitorType.map(windMtv)
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

  def historyTrendChart(epaMonitorStr: String, monitorTypeStr: String, reportUnitStr: String,
                        startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      import scala.collection.JavaConverters._
      val epaMonitors = if (epaMonitorStr.equalsIgnoreCase("None"))
        Array.empty[EpaMonitor.Value]
      else
        epaMonitorStr.split(':').map { EpaMonitor.withName }
      val monitorTypeStrArray = monitorTypeStr.split(':')
      val monitorTypes = monitorTypeStrArray.map { MonitorType.withName }
      val reportUnit = ReportUnit.withName(reportUnitStr)
      val (start, end) =
        if (reportUnit == ReportUnit.Hour) {
          (DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd HH:mm")),
            DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd HH:mm")))
        } else if (reportUnit == ReportUnit.Day) {
          (DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd")),
            DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd")))
        } else {
          (DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-M")),
            DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-M")))
        }

      val outputType = OutputType.withName(outputTypeStr)

      val chart = trendHelper(epaMonitors, monitorTypes, reportUnit, start, end)

      if (outputType == OutputType.excel) {
        import java.nio.file.Files
        val epaMts = epaMonitors.flatMap { _ => monitorTypes.toList }
        val excelFile = ExcelUtility.exportChartData(chart, epaMts)
        val downloadFileName =
          if (chart.downloadFileName.isDefined)
            chart.downloadFileName.get
          else
            chart.title("text")


        Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment(downloadFileName + ".xlsx", "UTF-8"),
          onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
      } else {
        Results.Ok(Json.toJson(chart))
      }
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
          records = Record.getEpaHourRecord(epa, monitorTypes(0), start, end).filter { r => r.value.isDefined }
          timeRecords = records.map { t => t.time -> t.value.get }
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
        if (monitorType == "")
          "風瑰圖"
        else {
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

  def pm25Query = Security.Authenticated {
    Ok(views.html.Pm25OverLawQuery())
  }
  
  object Pm25FilterType extends Enumeration {
    val Hour = Value("hour")
    val Day = Value("day")
  }

  import java.nio.file.Files
  def pm25OverLawReport(epaMonitorStr: String, filterTypeStr: String,
                        startStr: String, endStr: String, outputTypeStr:String) = Security.Authenticated {
    implicit request =>
      import scala.collection.JavaConverters._
      val epaMonitors = epaMonitorStr.split(':').map { EpaMonitor.withName }.toList
      val filterType = Pm25FilterType.withName(filterTypeStr)
      val (start, end) =
        (DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd")),
          DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd")))
      val outputType = OutputType.withName(outputTypeStr)
          
      val mt = MonitorType.withName("PM2.5")
      val period: Period =
        if (filterType == Pm25FilterType.Hour)
          1.hour
        else
          1.day

      val overLawRecords =
        if (filterType == Pm25FilterType.Hour) {
          for {
            m <- epaMonitors
            overLaw = Record.getEpaHourRecordOverCount(m, mt, start, end, 65)
          } yield (m, overLaw.get)
        } else {
          for {
            m <- epaMonitors
            recordMap = getPeriodReportMap(m, mt, period)(start, end)
          } yield {
            (m, recordMap.values.toList.filter { _ > 35 }.length)
          }
        }
      
      val order = overLawRecords.sortBy(_._2).reverse.zipWithIndex
      if(outputType == OutputType.html)
        Ok(views.html.Pm25OverLawReport(filterType, start, end, order))
      else{
        val excelFile = ExcelUtility.Pm25OverLawReport(filterType, start, end, order)
        Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment("PM25超標統計" + ".xlsx", "UTF-8"),
          onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
      }
  }

  def monitorTypeOverLaw = Security.Authenticated {
    Ok(views.html.monitorTypeOverLaw())
  }
  
  def mtOverLawReport(epaMonitorStr:String ,monitorTypeStr:String, lawStandardStr:String, 
      startStr:String, endStr:String, outputTypeStr:String)= Security.Authenticated {
      val epaMonitors = epaMonitorStr.split(':').map { EpaMonitor.withName }.toList
      val monitorType = MonitorType.withName(monitorTypeStr)
      val lawStandard = java.lang.Float.parseFloat(lawStandardStr)
      val (start, end) =
        (DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd")),
          DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd")))
      val outputType = OutputType.withName(outputTypeStr)

      val overLawRecords =
          for {
            m <- epaMonitors
            overLawOpt = Record.getEpaHourRecordOverCount(m, monitorType, start, end, lawStandard) if overLawOpt.isDefined
          } yield (m, overLawOpt.get)
      
      val order = overLawRecords.sortBy(_._2).reverse.zipWithIndex
      if(outputType == OutputType.html)
        Ok(views.html.monitorTypeOverLawReport(monitorType, lawStandard, start, end, order))
      else{
        val excelFile = ExcelUtility.monitorTypeOverLawReport(monitorType, lawStandard, start, end, order)
        Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment(s"${MonitorType.map(monitorType).desp}超標統計.xlsx", "UTF-8"),
          onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
      }
  }

  def psiQuery = Security.Authenticated {
    implicit request =>
    Ok(views.html.PsiQuery())
  }

  def psiOverLawReport(epaMonitorStr: String,
                       startStr: String, endStr: String, outputTypeStr:String) = Security.Authenticated {
    implicit request =>
      import scala.collection.JavaConverters._
      val epaMonitors = epaMonitorStr.split(':').map { EpaMonitor.withName }
      val (start, end) =
        (DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd")),
          DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd")))
      val outputType = OutputType.withName(outputTypeStr)

      val overLawList =
        for {
          m <- epaMonitors.toList
        } yield {
          val overLaw = Psi.getPsiOverLawCount(m, start, end, 100).get
          (m, overLaw)
        }

      val order = overLawList.sortBy(_._2).reverse.zipWithIndex
      if(outputType == OutputType.html)
        Ok(views.html.PsiOverLawReport(start, end, order))
      else{
        val excelFile = ExcelUtility.PsiOverLawReport(start, end, order)
        Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment("PSI超標統計" + ".xlsx", "UTF-8"),
          onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
      }
  }

  def sitePsiOrder = Security.Authenticated {
    Ok(views.html.SitePsiQuery())
  }

  def sitePsiOrderReport(startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    val (start, end) =
      (DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd")),
        DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd")))
    val outputType = OutputType.withName(outputTypeStr)

    val overLawList =
      for {
        m <- EpaMonitor.mvList
        overLaw = Psi.getPsiOverLawCount(m, start, end, 100).get
      } yield {
        (m, overLaw)
      }

    val order = overLawList.sortBy(_._2).reverse.zipWithIndex
    if (outputType == OutputType.html)
      Ok(views.html.PsiOverLawReport(start, end, order))
    else {
      val excelFile = ExcelUtility.PsiOverLawReport(start, end, order)
      Ok.sendFile(excelFile, fileName = _ =>
        play.utils.UriEncoding.encodePathSegment("PSI超標統計" + ".xlsx", "UTF-8"),
        onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
    }
  }

  def districtPsiOrder = Security.Authenticated {
    Ok(views.html.DistrictPsiQuery())
  }

  def districtPsiOrderReport(startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      import scala.collection.JavaConverters._
      
      val (start, end) =
        (DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd")),
          DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd")))
      val outputType = OutputType.withName(outputTypeStr)

      val overLawList =
        for {
          d <- District.list
          mList = EpaMonitor.mvList.filter { m => EpaMonitor.map(m).districtID.isDefined && 
            EpaMonitor.map(m).districtID.get == d.id } if mList.length > 0
        } yield {
          val overLaw = Psi.getPsiOverLawCount(mList, start, end, 100).get  
          val monitorNames = mList.map { EpaMonitor.map(_).name }.mkString(", ")
          (District(d.id), monitorNames, overLaw.toFloat/mList.length)
        }

      val order = overLawList.sortBy(_._2).reverse.zipWithIndex
      if (outputType == OutputType.html)
        Ok(views.html.DistrictPsiOverLawReport(start, end, order))
      else {
        val excelFile = ExcelUtility.DistrictPsiOverLawReport(start, end, order)
        Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment("行政區PSI超標統計" + ".xlsx", "UTF-8"),
          onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
      }
  }
    
  def districtOrder = Security.Authenticated {
    Ok(views.html.DistrictOrder())
  }
  
  def districtOrderReport(epaMonitorFilterStr:String, monitorTypeStr:String, startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      import scala.collection.JavaConverters._
      val epaMonitorFilter = epaMonitorFilterStr.split(":").map(EpaMonitorFilter.withName)
      val monitorType = MonitorType.withName(monitorTypeStr)
      val (start, end) =
        (DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd")),
          DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd")))
      val outputType = OutputType.withName(outputTypeStr)

      val orderList =
        for {
          d <- District.vList
          mList = EpaMonitor.normalMonitor.filter { EpaMonitorFilter.filters(epaMonitorFilter) }.
            filter {District.filter(d) } if mList.length > 0
        } yield {
          val avgList = mList.map{Record.getEpaDailyRecordAvg(_, monitorType, start, end)}
            .filter { _.isDefined }.map{_.get}
          val monitorNames = mList.map { EpaMonitor.map(_).name }.mkString(", ")
          if(avgList.length != 0)
            (District(d.id), monitorNames, Some(avgList.sum/avgList.length))
          else
            (District(d.id), monitorNames, None)
        }

      val order = orderList.sortBy(_._3).reverse.zipWithIndex
      val filterExplain = epaMonitorFilter.map { EpaMonitorFilter.map }.mkString(",")
      if (outputType == OutputType.html)
        Ok(views.html.orderReport(monitorType, filterExplain, start, end, order))
      else {
        val excelFile = ExcelUtility.DistrictOrderReport(monitorType, filterExplain, start, end, order)
        Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment(s"行政區${MonitorType.map(monitorType).desp}排名" + ".xlsx", "UTF-8"),
          onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
      }  
  }
  
  def monitorOrder = Security.Authenticated {
    Ok(views.html.monitorOrder())
  }
  
  def monitorOrderReport(epaMonitorFilterStr:String, monitorTypeStr:String, startStr: String, endStr: String, outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      import scala.collection.JavaConverters._
      val epaMonitorFilter = epaMonitorFilterStr.split(":").map(EpaMonitorFilter.withName)
      val monitorType = MonitorType.withName(monitorTypeStr)
      val (start, end) =
        (DateTime.parse(startStr, DateTimeFormat.forPattern("YYYY-MM-dd")),
          DateTime.parse(endStr, DateTimeFormat.forPattern("YYYY-MM-dd")))
      val outputType = OutputType.withName(outputTypeStr)

      val orderList =
        for {
          monitor<-EpaMonitor.normalMonitor.filter { EpaMonitorFilter.filters(epaMonitorFilter) }
          avg = Record.getEpaDailyRecordAvg(monitor, monitorType, start, end) if avg.isDefined
        } yield {
          (monitor, avg)
        }

      val order = orderList.sortBy(_._2).reverse.zipWithIndex
      val filterExplain = epaMonitorFilter.map { EpaMonitorFilter.map }.mkString(",")
      if (outputType == OutputType.html)
        Ok(views.html.monitorOrderReport(monitorType, filterExplain, start, end, order))
      else {
        val excelFile = ExcelUtility.monitorOrderReport(monitorType, filterExplain, start, end, order)
        Ok.sendFile(excelFile, fileName = _ =>
          play.utils.UriEncoding.encodePathSegment(s"測站${MonitorType.map(monitorType).desp}排名" + ".xlsx", "UTF-8"),
          onClose = () => { Files.deleteIfExists(excelFile.toPath()) })
      }  
  }

}