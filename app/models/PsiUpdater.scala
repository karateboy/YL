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

object PsiUpdater {
  def start(year:Int)={
    val worker = Akka.system.actorOf(Props[PsiUpdater], name = "PsiUpdater" + (Math.random()*1000).toInt)
    worker ! CalculatePsi(year)

  }
}

case class CalculatePsi(year:Int)
class PsiUpdater extends Actor{
  def receive = {
    case CalculatePsi(year)=>
      Logger.info(s"開始計算 $year 年PSI")
      updatePsi(year)
      Logger.info(s"結束計算 $year 年PSI")
      self ! PoisonPill
  }
  
  def updatePsi(year:Int)={
    val start = DateTime.parse(s"$year-1-1")
    val end = DateTime.parse(s"${year+1}-1-1")
    for(m <- EpaMonitor.mvList){
      Psi.updateDayPsi(m, start, end)
      Psi.updateRealtimePsi(m, start, end)
    }
    
  }
}