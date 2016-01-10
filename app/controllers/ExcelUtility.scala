package controllers
import play.api._
import play.api.Play.current
import controllers._
import models._
import org.apache.poi.openxml4j.opc._
import org.apache.poi.xssf.usermodel._
import com.github.nscala_time.time.Imports._
import java.io._
import java.nio.file.Files
import java.nio.file._
import org.apache.poi.ss.usermodel._

object ExcelUtility {
  val docRoot = "/report_template/"

  private def prepareTemplate(templateFile: String) = {
    val templatePath = Paths.get(current.path.getAbsolutePath + docRoot + templateFile)
    val reportFilePath = Files.createTempFile("temp", ".xlsx");

    Files.copy(templatePath, reportFilePath, StandardCopyOption.REPLACE_EXISTING)

    //Open Excel
    val pkg = OPCPackage.open(new FileInputStream(reportFilePath.toAbsolutePath().toString()))
    val wb = new XSSFWorkbook(pkg);

    (reportFilePath, pkg, wb)
  }

  def finishExcel(reportFilePath: Path, pkg: OPCPackage, wb: XSSFWorkbook) = {
    val out = new FileOutputStream(reportFilePath.toAbsolutePath().toString());
    wb.write(out);
    out.close();
    pkg.close();

    new File(reportFilePath.toAbsolutePath().toString())
  }
  
  def createStyle(mt: MonitorType.Value)(implicit wb: XSSFWorkbook) = {
    val prec = MonitorType.map(mt).prec
    val format_str = "0." + "0" * prec
    val style = wb.createCellStyle();
    val format = wb.createDataFormat();
        // Create a new font and alter it.
    val font = wb.createFont();
    font.setFontHeightInPoints(10);
    font.setFontName("標楷體");

    style.setFont(font)
    style.setDataFormat(format.getFormat(format_str))
    style.setBorderBottom(CellStyle.BORDER_THIN);
    style.setBottomBorderColor(IndexedColors.BLACK.getIndex());
    style.setBorderLeft(CellStyle.BORDER_THIN);
    style.setLeftBorderColor(IndexedColors.BLACK.getIndex());
    style.setBorderRight(CellStyle.BORDER_THIN);
    style.setRightBorderColor(IndexedColors.BLACK.getIndex());
    style.setBorderTop(CellStyle.BORDER_THIN);
    style.setTopBorderColor(IndexedColors.BLACK.getIndex());
    style
  }
    
  import controllers.Highchart._
  def exportChartData(chart: HighchartData, monitorTypes: Array[MonitorType.Value]): File = {
    val precArray = monitorTypes.map { mt => MonitorType.map(mt).prec }
    exportChartData(chart, precArray)
  }

  def exportChartData(chart: HighchartData, precArray: Array[Int]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("chart_export.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val format = wb.createDataFormat();

    val sheet = wb.getSheetAt(0)
    val headerRow = sheet.createRow(0)
    headerRow.createCell(0).setCellValue("時間")

    var pos = 0
    for {
      col <- 1 to chart.series.length
      series = chart.series(col - 1)
    } {
      headerRow.createCell(pos+1).setCellValue(series.name)
      pos+=1
    }

    val styles = precArray.map { prec =>
      val format_str = "0." + "0" * prec
      val style = wb.createCellStyle();
      style.setDataFormat(format.getFormat(format_str))
      style
    }

    // Categories data
    if (chart.xAxis.categories.isDefined) {
      val timeList = chart.xAxis.categories.get
      for (row <- timeList.zipWithIndex) {
        val rowNo = row._2 + 1
        val thisRow = sheet.createRow(rowNo)
        thisRow.createCell(0).setCellValue(row._1)

        for {
          col <- 1 to chart.series.length
          series = chart.series(col - 1)
        } {
          val cell = thisRow.createCell(col)
          cell.setCellStyle(styles(col - 1))

          val pair = series.data(rowNo - 1)
          if (pair.length == 2 && pair(1).isDefined) {
            cell.setCellValue(pair(1).get)
          }
          //val pOpt = series.data(rowNo-1)
          //if(pOpt.isDefined){
          //  cell.setCellValue(pOpt.get)
          //}

        }
      }
    } else {
      val rowMax = chart.series.map(s => s.data.length).max
      for (row <- 1 to rowMax) {
        val thisRow = sheet.createRow(row)
        val timeCell = thisRow.createCell(0)
        pos = 0
        for {
          col <- 1 to chart.series.length
          series = chart.series(col - 1)
         } {
          val cell = thisRow.createCell(pos +1)
          pos +=1
          cell.setCellStyle(styles(col - 1))

          val pair = series.data(row - 1)
          if (col == 1) {
            val dt = new DateTime(pair(0).get.toLong)
            timeCell.setCellValue(dt.toString("YYYY/MM/dd HH:mm"))
          }
          if (pair(1).isDefined) {
            cell.setCellValue(pair(1).get)
          }                    
        }
      }
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  import Query._
  def Pm25OverLawReport(filterType:Pm25FilterType.Value, start:DateTime, end:DateTime, records:List[((EpaMonitor.Value, Int), Int)])={
    implicit val (reportFilePath, pkg, wb) = prepareTemplate("report.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    
    val sheet = wb.getSheetAt(0)
    val headerRow = sheet.getRow(0)
    if(filterType == Pm25FilterType.Hour){
      headerRow.getCell(0).setCellValue("PM2.5超標統計,小時濃度>65")
      sheet.getRow(2).getCell(2).setCellValue("超標小時數")
    }else{
      headerRow.getCell(0).setCellValue("PM2.5超標統計,日平均>35")
      sheet.getRow(2).getCell(2).setCellValue("超標日數")
    }
    
    sheet.getRow(1).getCell(0).setCellValue(s"區間:${start.toString("YYYY-MM-dd HH:mm")}~${end.toString("YYYY-MM-dd HH:mm")}")
    
    for(r <- records){      
      val rowN = r._2 + 3
      val row = sheet.createRow(rowN)
      row.createCell(0).setCellValue(r._2+1)
      row.createCell(1).setCellValue(EpaMonitor.map(r._1._1).name)
      row.createCell(2).setCellValue(r._1._2)
    }
    finishExcel(reportFilePath, pkg, wb)
  }
  
  def PsiOverLawReport(start:DateTime, end:DateTime, records:List[((EpaMonitor.Value, Int), Int)])={
    implicit val (reportFilePath, pkg, wb) = prepareTemplate("report.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val style = createStyle(MonitorType.withName("SO2"))
    val sheet = wb.getSheetAt(0)
    val headerRow = sheet.getRow(0)
      headerRow.getCell(0).setCellValue("PSI超標統計,日PSI>100")
      sheet.getRow(2).getCell(2).setCellValue("超標日數")
    
    sheet.getRow(1).getCell(0).setCellValue(s"區間:${start.toString("YYYY-MM-dd HH:mm")}~${end.toString("YYYY-MM-dd HH:mm")}")
    
    for(r <- records){      
      val rowN = r._2 + 3
      val row = sheet.createRow(rowN)
      row.createCell(0).setCellValue(r._2+1)
      row.createCell(1).setCellValue(EpaMonitor.map(r._1._1).name)
      row.createCell(2).setCellValue(r._1._2)
    }
    finishExcel(reportFilePath, pkg, wb)
  }

  def DistrictPsiOverLawReport(start:DateTime, end:DateTime, records: List[((District.Value, Float), Int)])={
    implicit val (reportFilePath, pkg, wb) = prepareTemplate("districtReport.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val style = createStyle(MonitorType.withName("SO2"))
    val sheet = wb.getSheetAt(0)
    val headerRow = sheet.getRow(0)
    headerRow.getCell(0).setCellValue("PSI超標統計,日PSI>100")
    sheet.getRow(2).getCell(3).setCellValue("平均超標日數")
    sheet.getRow(1).getCell(0).setCellValue(s"區間:${start.toString("YYYY-MM-dd HH:mm")}~${end.toString("YYYY-MM-dd HH:mm")}")
    
    for(r <- records){      
      val rowN = r._2 + 3
      val row = sheet.createRow(rowN)
      row.createCell(0).setCellStyle(style)
      row.getCell(0).setCellValue(r._2+1)
      row.createCell(1).setCellStyle(style)
      row.getCell(1).setCellValue(District.map(r._1._1).name)
      row.createCell(2).setCellStyle(style)
      row.getCell(2).setCellValue(District.getEpaMonitorNameStr(r._1._1))
      row.createCell(3).setCellStyle(style)
      row.getCell(3).setCellValue(r._1._2)
    }
    finishExcel(reportFilePath, pkg, wb)
  }
  
  def DistrictOrderReport(mt:MonitorType.Value,start:DateTime, end:DateTime, records: List[((District.Value, Option[Float]), Int)])={
    implicit val (reportFilePath, pkg, wb) = prepareTemplate("districtReport.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    
    val style = createStyle(mt)
    val sheet = wb.getSheetAt(0)
    val headerRow = sheet.getRow(0)
    headerRow.getCell(0).setCellValue(s"行政轄區${MonitorType.map(mt).desp}排序:")
    sheet.getRow(2).getCell(3).setCellValue("平均值")
    sheet.getRow(1).getCell(0).setCellValue(s"區間:${start.toString("YYYY-MM-dd HH:mm")}~${end.toString("YYYY-MM-dd HH:mm")}")
    
    
    for(r <- records){      
      val rowN = r._2 + 3
      val row = sheet.createRow(rowN)
      row.createCell(0).setCellStyle(style)
      row.getCell(0).setCellValue(r._2+1)
      row.createCell(1).setCellStyle(style)
      row.getCell(1).setCellValue(District.map(r._1._1).name)
      row.createCell(2).setCellStyle(style)
      row.getCell(2).setCellValue(District.getEpaMonitorNameStr(r._1._1))
      row.createCell(3).setCellStyle(style)
      if(r._1._2.isDefined){			
				row.getCell(3).setCellValue(r._1._2.get)
			}else{
				row.getCell(3).setCellValue("-")
			}      
    }
    finishExcel(reportFilePath, pkg, wb)
  }
}