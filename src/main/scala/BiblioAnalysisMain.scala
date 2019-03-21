import scala.io.Source

object BiblioAnalysisMain extends App {

  case class Pub(var authors: String, var title: String, var publication: String)

  println("Contents")

  val src = Source.fromFile("/Users/yilmaz/IdeaProjects/" +
    "BiblioAnalysis/savedrecs.ciw")
  var count : Int = 0

  src.mkString split("PT J\n") foreach {x => {
    println("**********")

    count = count+1
    println(x)
  }}
/*
  src.getLines() foreach {x => {
    //println(x)
    if (x.startsWith("PT")) count= count+1
  }}
*/
  println("Number of records is " + count)

}
