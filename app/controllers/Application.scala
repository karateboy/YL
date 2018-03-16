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

case class EpaRealtimeData(
  aqi: String,
  co: String,
  co_8hr: String,
  county: String,
  no: String,
  no2: String,
  nox: String,
  o3: String,
  o3_8hr: String,
  pm10: String,
  pm10_avg: String,
  pm25: String,
  pm25_avg: String,
  pollutant: String,
  publishTime: String,
  siteName: String,
  so2: String,
  status: String,
  windDirec: String,
  windSpeed: String,
  var siteType: Option[String] = None)

object Application extends Controller {
  import scala.concurrent.ExecutionContext.Implicits.global
  
  implicit val epaRealtimeDataRead: Reads[EpaRealtimeData] =
    ((__ \ "AQI").read[String] and
      (__ \ "CO").read[String] and
      (__ \ "CO_8hr").read[String] and
      (__ \ "County").read[String] and
      (__ \ "NO").read[String] and
      (__ \ "NO2").read[String] and
      (__ \ "NOx").read[String] and
      (__ \ "O3").read[String] and
      (__ \ "O3_8hr").read[String] and
      (__ \ "PM10").read[String] and
      (__ \ "PM10_AVG").read[String] and
      (__ \ "PM2.5").read[String] and
      (__ \ "PM2.5_AVG").read[String] and
      (__ \ "Pollutant").read[String] and
      (__ \ "PublishTime").read[String] and
      (__ \ "SiteName").read[String] and
      (__ \ "SO2").read[String] and
      (__ \ "Status").read[String] and
      (__ \ "WindDirec").read[String] and
      (__ \ "WindSpeed").read[String] and
      (__ \ "siteType").readNullable[String])(EpaRealtimeData.apply _)

  def index = Action.async {
    implicit request =>
      {
        val url2 = "https://opendata.epa.gov.tw/webapi/api/rest/datastore/{resourceID}/?format={format}&limit={limit}&token={token}"
        val url = "http://opendata2.epa.gov.tw/AQI.json"
        val sites = List("二林", "線西", "崙背", "斗六", "臺西", "麥寮", "竹山", "嘉義", "朴子")
        val siteTypeMap = Map(
          "二林" -> "一般測站",
          "線西" -> "工業測站",
          "崙背" -> "一般測站",
          "斗六" -> "一般測站",
          "臺西" -> "工業測站",
          "麥寮" -> "工業測站",
          "竹山" -> "一般測站",
          "嘉義" -> "一般測站",
          "朴子" -> "一般測站")
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
                  val rt_data = data.filter { d => sites.contains(d.siteName) }.map {
                    info =>
                      info.siteType = Some(siteTypeMap(info.siteName))
                      info
                  }.sortBy { d => sites.indexOf(d.siteName) }
                  Ok(views.html.realtime(rt_data))
                })
            } catch {
              case ex: Exception =>
                Logger.error(ex.toString())
                Ok(views.html.realtime(Seq.empty[EpaRealtimeData]))
            }
        }
      }
  }

  def siteInfo = Security.Authenticated {
    Ok(views.html.siteInfo())
  }

  def importEpa = Security.Authenticated {
    EpaDataImporter.importYesterday
    Ok("")
  }

  def importDataAfter(dateStr: String) = Security.Authenticated {
    val date = DateTime.parse(dateStr)
    EpaDataImporter.importDataAfter(date)
    Ok(s"import after ${date.toString()}")
  }

  val path = current.path.getAbsolutePath + "/importEPA/"

  def importEpa103 = Action {
    Epa103Importer.importData(path)
    Ok(s"匯入 $path")
  }

  def importEpa97 = Action {
    Epa97Importer.importData(path)
    Ok(s"匯入 $path")
  }

  def importEpa100 = Action {
    Epa100Importer.importData(path)
    Ok(s"匯入 $path")
  }

  def updatePSI(year: Int) = Action {
    PsiUpdater.start(year)
    Ok(s"$year 年PSI計算中...")
  }

  def importPSI = Action {
    PsiImporter.importData(path)
    Ok(s"匯入PSI...")
  }
}
