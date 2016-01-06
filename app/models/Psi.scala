package models
import scalikejdbc._
import play.api._
import play.api.mvc._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import java.sql.Timestamp

case class PsiRecord(m:EpaMonitor.Value, date:DateTime, value:Float)
object Psi {
  def getPsiDayRecord(m:EpaMonitor.Value, start:DateTime, end:DateTime)={
    val startT:Timestamp = start
    val endT:Timestamp = end
    DB readOnly{
      implicit session=>
        sql"""
          SELECT *
          FROM [YLDB].[dbo].[PsiDay]
          Where Date >= ${startT} and Date < ${endT}
          """.map { r => PsiRecord(EpaMonitor.idMap(r.int(1)), r.timestamp(2), r.float(3)) }.list.apply
    }
  }
  
  def getPsiHourRecord(m:EpaMonitor.Value, start:DateTime, end:DateTime)={
    val startT:Timestamp = start
    val endT:Timestamp = end
    DB readOnly{
      implicit session=>
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

}