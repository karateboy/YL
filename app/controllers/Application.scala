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

  def siteInfo  = Security.Authenticated {
    Ok(views.html.siteInfo())
  }
  
  def importEpa = Security.Authenticated{
    EpaDataImporter.importYesterday
    Ok("")
  }
  
  val path = current.path.getAbsolutePath + "/importEPA/"
  
  def importEpa103 = Action{    
    Epa103Importer.importData(path)
    Ok(s"匯入 $path")
  }
  
  def importEpa100 = Action{
    Epa100Importer.importData(path)
    Ok(s"匯入 $path")    
  }
  
  def updatePSI(year:Int) = Action{
    PsiUpdater.start(year)
    Ok(s"$year 年PSI計算中...")
  }
  
  def importPSI = Action{
    PsiImporter.importData(path)
    Ok(s"匯入PSI...")    
  }
}
