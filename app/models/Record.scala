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
  
  def getMtRose(monitor: EpaMonitor.Value, monitorType:MonitorType.Value, start: DateTime, end: DateTime, level:List[Float], nDiv: Int = 16) = {
    val mt_values = getEpaHourRecord(monitor, monitorType, start, end)
    val wind_dirs = getEpaHourRecord(monitor, MonitorType.C212, start, end)
    val windRecords = wind_dirs.zip(mt_values)

    val step = 360f / nDiv
    import scala.collection.mutable.ListBuffer
    val windDirPair =
      for (d <- 0 to nDiv - 1) yield {
        (d -> ListBuffer[Float]())
      }
    val windMap = Map(windDirPair: _*)

    var total = 0
    for (w <- windRecords) {
      if (w._1.time == w._2.time) {
        val dir = (Math.ceil((w._1.value - (step/2)) / step).toInt)% nDiv
        windMap(dir) += w._2.value
        total += 1
      }
    }

    def winSpeedPercent(winSpeedList: ListBuffer[Float]) = {
      val count = new Array[Float](level.length+1)
      def getIdx(v:Float):Int={
        for(i <- 0 to level.length-1){
          if(v < level(i))
            return i
        }
        
        return level.length
      }
      
      for (w <- winSpeedList) {
        val i = getIdx(w)
        count(i) +=1
      }

      assert(total != 0)
      count.map(_ * 100 / total)
    }

    windMap.map(kv => (kv._1, winSpeedPercent(kv._2)))
  }

}