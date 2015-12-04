package models

/**
 * @author user
 */
case class EpaMonitor(name:String, id:Int)
object EpaMonitor extends Enumeration{
  val Erlin = Value("Erlin")
  val XianXi = Value("XianXi")
  val Lunbei = Value("Lunbei")
  val DoLiao = Value("DoLiao")
  val Taixi = Value("Taixi")
  val Mailiao = Value("Mailiao")
  val ZuSan = Value("ZuSan")
  val Jaii = Value("Jaii")
  val Puzi = Value("Puzi")
    
  
  val map=Map(
    Erlin->EpaMonitor("環保署二林站", 35),
    Puzi->EpaMonitor("環保署朴子站", 40),
    Lunbei->EpaMonitor("環保署崙背站", 38),
    Taixi->EpaMonitor("環保署臺西站", 41),
    Mailiao->EpaMonitor("環保署麥寮站", 83),
	XianXi->EpaMonitor("環保署線西站", 34),
	DoLiao->EpaMonitor("環保署斗六站", 37),
	ZuSan->EpaMonitor("環保署竹山站", 69),
	Jaii->EpaMonitor("環保署嘉義站", 42)
  )
  
  val idMap = map.map(r=>(r._2.id, r._1))
  
  val epaList = values.toList.sorted
}