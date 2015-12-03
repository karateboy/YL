package models
import java.sql.Date
import java.sql.Timestamp
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._

object Record {
  case class EpaHourRecord(monitor: EpaMonitor.Value, time: DateTime, monitorType: MonitorType.Value, value: Float)
  def getEpaHourRecord(epaMonitor: EpaMonitor.Value, monitorType: MonitorType.Value, startTime: DateTime, endTime: DateTime)(implicit session: DBSession = AutoSession) = {
    val start: Timestamp = startTime
    val end: Timestamp = endTime
    val monitorId = EpaMonitor.map(epaMonitor).id
    val monitorTypeStrOpt = MonitorType.map(monitorType).epa_mapping
    if (monitorTypeStrOpt.isEmpty)
      List.empty[EpaHourRecord]
    else {
      val monitorTypeStr = monitorTypeStrOpt.get
      sql"""
        Select * 
        From hour_data
        Where MStation=${monitorId} and MItem=${monitorTypeStr} and MDate >= ${start} and MDate < ${end}
        ORDER BY MDate ASC
      """.map {
        rs => EpaHourRecord(EpaMonitor.idMap(rs.int(2)), rs.timestamp(3), MonitorType.epaMap(rs.string(4)), rs.float(5))
      }.list().apply()
    }
  }
}