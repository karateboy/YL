package models
import scalikejdbc._
import play.api._
import play.api.mvc._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import java.sql.Timestamp
import models._

case class PsiRecord(m: EpaMonitor.Value, date: DateTime,
                     co: Option[Int], o3: Option[Int], no2: Option[Int], so2: Option[Int], pm10: Option[Int],
                     psi: Option[Int])
                     
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
          """.map { r =>
          PsiRecord(EpaMonitor.idMap(r.int(1)), r.timestamp(2),
            r.intOpt(3), r.intOpt(4), r.intOpt(5), r.intOpt(6), r.intOpt(7), r.intOpt(8))
        }.list.apply
    }
  }

  def getPsiOverLawCount(m: EpaMonitor.Value, start: DateTime, end: DateTime, standard:Float) = {
    val startT: Timestamp = start
    val endT: Timestamp = end
    DB readOnly {
      implicit session =>
        sql"""
          SELECT count(*)
          FROM [YLDB].[dbo].[PsiDay]
          Where Date >= ${startT} and Date < ${endT} and PSI > ${standard} and Station = ${EpaMonitor.map(m).id}
          """.map { r => r.int(1) }.single.apply()
    }
  }
  
  def getPsiOverLawCount(mList: List[EpaMonitor.Value], start: DateTime, end: DateTime, standard:Float) = {
    val startT: Timestamp = start
    val endT: Timestamp = end
    val idList = SQLSyntax.createUnsafely(s"${mList.map { EpaMonitor.map(_).id }.mkString(",")}")
    DB readOnly {
      implicit session =>
        sql"""
          SELECT count(*)
          FROM [YLDB].[dbo].[PsiDay]
          Where Date >= ${startT} and Date < ${endT} and PSI > ${standard} and Station in (${idList})
          """.map { r => r.int(1) }.single.apply()
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
           ,[CO]
           ,[O3]
           ,[NO2]
           ,[SO2]
           ,[PM10]
           ,[PSI])        
           VALUES
           (${EpaMonitor.map(pr.m).id}
           ,${date}
           ,${pr.co}
           ,${pr.o3}
           ,${pr.no2}
           ,${pr.so2}
           ,${pr.pm10}
           ,${pr.psi})
          """.update.apply
    }
  }
}