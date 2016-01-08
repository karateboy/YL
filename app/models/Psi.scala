package models
import scalikejdbc._
import play.api._
import play.api.mvc._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import java.sql.Timestamp
import models._

case class PsiRecord(m: EpaMonitor.Value, date: DateTime, value: Float)
object Psi {
  def getPsiDayRecord(m: EpaMonitor.Value, start: DateTime, end: DateTime) = {
    val startT: Timestamp = start
    val endT: Timestamp = end
    DB readOnly {
      implicit session =>
        sql"""
          SELECT *
          FROM [YLDB].[dbo].[PsiDay]
          Where Date >= ${startT} and Date < ${endT}
          """.map { r => PsiRecord(EpaMonitor.idMap(r.int(1)), r.timestamp(2), r.float(3)) }.list.apply
    }
  }

  def getPsiHourRecord(m: EpaMonitor.Value, start: DateTime, end: DateTime) = {
    val startT: Timestamp = start
    val endT: Timestamp = end
    DB readOnly {
      implicit session =>
        sql"""
          SELECT *
          FROM [YLDB].[dbo].[PsiHour]
          Where Date >= ${startT} and Date < ${endT}
          """.map { r => PsiRecord(EpaMonitor.idMap(r.int(1)), r.timestamp(2), r.float(3)) }.list.apply
    }
  }

  def insertPsiDayRecord(pr: PsiRecord) = {
    val date: Timestamp = pr.date
    DB localTx {
      implicit session =>
        sql"""
          INSERT INTO [dbo].[PsiDay]
           ([Station]
           ,[Date]
           ,[PSI])
         VALUES
           (${EpaMonitor.map(pr.m).id}
           ,${date}
           ,${pr.value})
          """.update.apply
    }
  }

  def insertPsiHourRecord(pr: PsiRecord) = {
    val date: Timestamp = pr.date
    DB localTx {
      implicit session =>
        sql"""
          INSERT INTO [dbo].[PsiHour]
           ([Station]
           ,[Date]
           ,[PSI])
         VALUES
           (${EpaMonitor.map(pr.m).id}
           ,${date}
           ,${pr.value})
          """.update.apply
    }
  }

  import Record._
  def getMonitorTypeAvg(monitor: EpaMonitor.Value, monitorType: MonitorType.Value, start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    val records = getEpaHourRecord(monitor, monitorType, start, end)
    val typeValues = records.map { _.value }
    val duration = new Duration(start, end)
    val nHour = duration.getStandardHours
    val validValues = typeValues.filter(_.isDefined).map(_.get)
    val total = validValues.length
    if (total == 0)
      None
    else {
      val sum = validValues.sum
      Some(sum / total)
    }
  }

  def getMonitorTypeAvg(records: List[EpaHourRecord], monitorType: MonitorType.Value) = {
    val typeValues = records.map { _.value }
    val validValues = typeValues.filter(_.isDefined).map(_.get)
    val total = validValues.length
    if (total == 0)
      None
    else {
      Some(validValues.sum / total)
    }
  }

  def getMonitorTypeMax(monitor: EpaMonitor.Value, monitorType: MonitorType.Value, start: DateTime, end: DateTime) = {
    val typeValues = getEpaHourRecord(monitor, monitorType, start, end).map { _.value }
    val validValues = typeValues.filter(_.isDefined).map(_.get)
    val total = validValues.length
    if (total == 0)
      None
    else {
      Some(validValues.max)
    }
  }

  def getMonitorType8HourAvgMax(monitor: EpaMonitor.Value, monitorType: MonitorType.Value, start: DateTime, end: DateTime)(implicit session: DBSession = AutoSession) = {
    def EightHourAvg(start: DateTime): List[Option[Float]] = {
      if (start + 8.hour >= end)
        Nil
      else
        getMonitorTypeAvg(monitor, monitorType, start, end) :: EightHourAvg(start + 1.hours)
    }

    val avgs = EightHourAvg(start)

    avgs.max
  }

  def pm10PSI(ov: Option[Float]) = {
    if (ov.isEmpty)
      None
    else
      Some {
        val v = ov.get
        if (v >= 0 && v <= 50) {
          v
        } else if (v <= 100) {
          50 + (v - 50) * 50 / 100
        } else if (v <= 350) {
          100 + (v - 150) * 100 / (350 - 150)
        } else if (v <= 420) {
          200 + (v - 350) * 100 / (420 - 350)
        } else if (v <= 500) {
          300 + (v - 420) * 100 / (500 - 420)
        } else {
          400 + (v - 500) * 100 / 100
        }
      }
  }

  def so2PSI(ov: Option[Float]) = {
    if (ov.isEmpty)
      None
    else
      Some {
        val v = ov.get

        if (v <= 30) {
          v / 30 * 50
        } else if (v <= 140) {
          50 + (v - 30) * 50 / (140 - 30)
        } else if (v <= 300) {
          100 + (v - 140) * 100 / (300 - 140)
        } else if (v <= 600) {
          200 + (v - 300) * 100 / (600 - 300)
        } else if (v <= 800) {
          300 + (v - 600) * 100 / (800 - 600)
        } else {
          400 + (v - 800) * 100 / (1000 - 800)
        }
      }
  }

  def coPSI(ov: Option[Float]) = {
    if (ov.isEmpty)
      None
    else
      Some {
        val v = ov.get

        if (v <= 4.5) {
          v / 4.5f * 50f
        } else if (v <= 9) {
          (50 + (v - 4.5) * 50 / (9 - 4.5)).toFloat
        } else if (v <= 15) {
          100 + (v - 9) * 100 / (15 - 9)
        } else if (v <= 30) {
          200 + (v - 15) * 100 / (30 - 9)
        } else if (v <= 40) {
          300 + (v - 30) * 100 / (40 - 30)
        } else {
          400 + (v - 40) * 100 / (50 - 40)
        }
      }
  }

  def o3PSI(ov: Option[Float]) = {
    if (ov.isEmpty)
      None
    else
      Some {
        val v = ov.get

        if (v <= 60) {
          v / 60 * 50
        } else if (v <= 120) {
          50 + (v - 60) * 50 / (120 - 60)
        } else if (v <= 200) {
          100 + (v - 120) * 100 / (200 - 120)
        } else if (v <= 400) {
          200 + (v - 200) * 100 / (400 - 200)
        } else if (v <= 500) {
          300 + (v - 400) * 100 / (500 - 400)
        } else {
          400 + (v - 500) * 100 / (600 - 500)
        }
      }
  }

  def no2PSI(ov: Option[Float]) = {
    if (ov.isEmpty)
      None
    else
      Some {
        val v = ov.get

        if (v < 600) {
          0
        } else if (v <= 1200) {
          200 + (v - 600) * 100 / (1200 - 600)
        } else if (v <= 1600) {
          300 + (v - 1200) * 100 / (1600 - 1200)
        } else {
          400 + (v - 1600) * 100 / (2000 - 1600)
        }
      }
  }

  def getRealtimePSI(monitor: EpaMonitor.Value, current: DateTime)(implicit session: DBSession = AutoSession) = {
    val pm10_12 = getMonitorTypeAvg(monitor, MonitorType.withName("PM10"), current - 11.hour, current + 1.hour)
    val pm10_4 = getMonitorTypeAvg(monitor, MonitorType.withName("PM10"), current - 3.hour, current + 1.hour)
    val pm10 = if (pm10_12.isDefined && pm10_4.isDefined)
      Some((pm10_12.get + pm10_4.get) / 2)
    else
      None

    val so2_24 = getMonitorTypeAvg(monitor, MonitorType.withName("SO2"), current - 23.hour, current + 1.hour)
    val co_8 = getMonitorTypeAvg(monitor, MonitorType.withName("CO"), current - 7.hour, current + 1.hour)
    val o3 = getMonitorTypeAvg(monitor, MonitorType.withName("O3"), current, current + 1.hour)
    val no2 = getMonitorTypeAvg(monitor, MonitorType.withName("NO2"), current, current + 1.hour)
    val result = Map[MonitorType.Value, (Option[Float], Option[Float])](
      MonitorType.withName("PM10") -> (pm10, pm10PSI(pm10)),
      MonitorType.withName("SO2") -> (so2_24, so2PSI(so2_24)),
      MonitorType.withName("CO") -> (co_8, coPSI(co_8)),
      MonitorType.withName("O3") -> (o3, o3PSI(o3)),
      MonitorType.withName("NO2") -> (no2, no2PSI(no2)))
    val sub_psi = result.values.map(_._2)
    val psi = sub_psi.toList.max

    PsiReport(psi, result)
  }

  case class PsiReport(psi: Option[Float], sub_map: Map[MonitorType.Value, (Option[Float], Option[Float])])

  def getMonthlyPSI(monitor: EpaMonitor.Value, start: DateTime) = {
    val end = start + 1.month

    def helper(day: DateTime): List[PsiReport] = {
      if (day >= end)
        Nil
      else
        getDailyPSI(monitor, day) :: helper(day + 1.day)
    }

    helper(start)
  }

  def getDailyPSI(m: EpaMonitor.Value, current: DateTime)(implicit session: DBSession = AutoSession) = {
    val start = current
    val end = current + 1.day

    val pm10_24 = getMonitorTypeAvg(m, MonitorType.withName("PM10"), start, end)
    val so2_24 = getMonitorTypeAvg(m, MonitorType.withName("SO2"), start, end)
    val o3 = getMonitorTypeMax(m, MonitorType.withName("O3"), start, end)
    val no2 = getMonitorTypeMax(m, MonitorType.withName("NO2"), start, end)

    val co_8 = getMonitorType8HourAvgMax(m, MonitorType.withName("CO"), current, current + 1.day)
    val result = Map[MonitorType.Value, (Option[Float], Option[Float])](
      MonitorType.withName("PM10") -> (pm10_24, pm10PSI(pm10_24)),
      MonitorType.withName("SO2") -> (so2_24, so2PSI(so2_24)),
      MonitorType.withName("CO") -> (co_8, coPSI(co_8)),
      MonitorType.withName("O3") -> (o3, o3PSI(o3)),
      MonitorType.withName("NO2") -> (no2, no2PSI(no2)))
    val sub_psi = result.values.map(_._2)
    val psi = sub_psi.toList.max

    PsiReport(psi, result)
  }

  import controllers.Query._
  def updateRealtimePsi(m: EpaMonitor.Value, start: DateTime, end: DateTime) = {
    val psiList =
      for (t <- getPeriods(start, end, 1.hour)) yield {
        Logger.info(s"計算 ${EpaMonitor.map(m).name} ${t.toString("YYYY-MM-dd")} 即時PSI")
        val ts: java.sql.Timestamp = t
        Seq(EpaMonitor.map(m).id, ts, getRealtimePSI(m, t).psi)
      }

    DB autoCommit { implicit session =>
      sql"""
        INSERT INTO [dbo].[PsiHour]
           ([Station]
           ,[Date]
           ,[Psi])
        values (
        ?,?,?)
        """
        .batch(psiList.toSeq: _*)
        .apply()
    }

  }

  def updateDayPsi(m: EpaMonitor.Value, start: DateTime, end: DateTime) = {
    val psiList =
      for (t <- getPeriods(start, end, 1.day)) yield {
        Logger.info(s"計算 ${EpaMonitor.map(m).name} ${t.toString("YYYY-MM-dd")} 日PSI")
        val ts: java.sql.Timestamp = t
        Seq(EpaMonitor.map(m).id, ts, getDailyPSI(m, t).psi)
      }

    DB autoCommit { implicit session =>
      sql"""
        INSERT INTO [dbo].[PsiDay]
           ([Station]
           ,[Date]
           ,[PSI])

        values (
        ?,?,?)
        """
        .batch(psiList.toSeq: _*)
        .apply()
    }
  }

}