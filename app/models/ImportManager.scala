package models
import play.api._
import akka.actor._
import com.github.nscala_time.time.Imports._
import play.api.Play.current
import play.api.libs.concurrent.Akka


class ImportManager extends Actor {
  def receive = {
    case ImportYesterday =>
      EpaDataImporter.importYesterday
  }
}