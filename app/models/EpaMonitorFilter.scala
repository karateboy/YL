package models

object EpaMonitorFilter extends Enumeration{
  val ORDINARY = Value("ordinary")
  val INDUSTRIAL = Value("industrial")
  val TRAFFIC = Value("traffic")
  val PARK = Value("park")
  val OTHER = Value("other")
  
  val map = Map(ORDINARY->"一般測站", INDUSTRIAL->"工業測站", TRAFFIC->"交通測站", PARK->"公園測站", OTHER->"其他測站")
  def filter(f:EpaMonitorFilter.Value)={m:EpaMonitor.Value=>
    f match{
      case ORDINARY=>
        EpaMonitor.map(m).ordinary
      case INDUSTRIAL=>
        EpaMonitor.map(m).industrial
      case TRAFFIC=>
        EpaMonitor.map(m).traffic
      case PARK=>
        EpaMonitor.map(m).park
      case OTHER=>
        EpaMonitor.map(m).other
    }
  }
  
  def filters(fs:Seq[EpaMonitorFilter.Value])(m:EpaMonitor.Value):Boolean={
    def op(a:Boolean,b:Boolean)={
      a || b
    }
    
    fs.map{filter(_)(m)}.fold(false){op}

  }
}