import scala.io.Source

object BiblioAnalysisMain extends App {

  def getAuthors(authorData: String) : String = {
    authorData.substring(3)
  }

  case class Pub(var authors: String, var title: String, var publication: String)

  println("Contents")

  val src = Source.fromFile("/Users/yilmaz/IdeaProjects/" +
    "BiblioAnalysis/savedrecs.ciw")
  var count : Int = 0

  src.mkString split("PT J\n") foreach {x => {
    println("**********")
    var authors : String = null
    var title : String = null
    var publication : String = null

    val lines = x split ("\n")
    lines foreach { l => {
      var currentCode : String = ""
      var code : String = ""
      if (l.length >= 2)
        code = l.substring(0,2)
        code match {
          case "AF" => {authors = getAuthors(l); println(authors);currentCode="AF"}
          case "TI" =>
          case _ => {
            if (currentCode == "AF") authors = authors + l
          }
          }

        }

    }}
    count = count+1
  }

  println("Number of records is " + count)

}
