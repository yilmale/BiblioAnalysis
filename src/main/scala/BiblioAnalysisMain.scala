import scala.io.Source

object BiblioAnalysisMain extends App {

  def getAuthors(authorData: String) : String = {
    authorData.substring(3)
  }

  def getTitle(article: String) : String = {
    article.substring(3)
  }

  def getSource(source: String) : String = {
    source.substring(3)
  }

  def getKeywords(kwds: String) : Array[String] = {
    kwds.split(";") map(_.stripLeading())
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
    var src : String = null
    var keywords: Array[String] = null

    val lines = x split ("\n")
    var currentCode : String = ""
    var code : String = ""
    lines foreach { l => {
      if (l.length >= 2)
        code = l.substring(0,2)
        code match {
          case "AF" => {authors = getAuthors(l);currentCode="AF"}
          case "TI" => {title = getTitle(l);currentCode="TI"}
          case "SO" => {src = getSource(l);currentCode="SO"}
          case "DE" =>
          case "  " => {
            if (currentCode == "AF") authors = authors + "\n" + l.stripLeading()
            else
            if (currentCode == "TI") title = title + " " + l.stripLeading()
            else
            if (currentCode == "SO") src = src + " " + l.stripLeading()
          }
          case _ => currentCode = code
          }

        }

    }
    println(authors)
    println(title)
    println(src)
  }
    count = count+1
  }

  println("Number of records is " + count)

}
