import scala.io.Source
import Array._

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
    var words = kwds.split(";") map(_.stripLeading())
    if (words(0).startsWith("DE"))
      words(0)=words(0).substring(3)
    words
  }

  def getRef(reference: String) : Ref = {
    var ref = reference.split(",")
    var au: String = ""
    var date: String = ""
    var src: String = ""
    if (ref.length > 0) {au  = ref(0)}
    if (ref.length > 1) {date = ref(1)}
    if (ref.length > 2) {src  = ref(2)}
    if (ref.length > 3) {
      for (j <- 3 until ref.length) src = src + " " + ref(j)
    }
    Ref(au,date,src)
  }

  def reverse(lst: List[Pub]) : List[Pub] = {
    lst match {
      case List() => lst
      case x :: xs1 => reverse(xs1) ::: List(x)
    }
  }



  case class Pub(var authors: String, var title: String, var publication: String,
                 var keywords: Array[String], var refs: List[Ref]) {
    override def toString(): String = {
      var str = s"Authors: $authors \n Title: $title \n Source: $publication\n"
      str =str+s"Keywords: "
      if (keywords != null) {keywords foreach {x => {str=str+s"$x\t"}}}
      var count = refs.length
      str=str+s"\nThere are $count references"
      str
    }
  }
  case class Ref(var authors: String, var date: String, var source: String)

  val srcData = Source.fromFile("/Users/yilmaz/IdeaProjects/" +
    "BiblioAnalysis/savedrecs.ciw")
  var count : Int = 0
  var pubs : List[Pub] = List()

  var authors : String = null
  var title : String = null
  var src : String = null
  var keywords: Array[String] = null
  var references : List[Ref] = List()

  srcData.mkString split("PT J\n") foreach {x => {
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
          case "DE" => {keywords = getKeywords(l);currentCode="DE"}
          case "CR" => {references = getRef(l) :: references;currentCode="CR"}
          case "  " => {
            if (currentCode == "AF") authors = authors + "\n" + l.stripLeading()
            else if (currentCode == "TI") title = title + " " + l.stripLeading()
            else if (currentCode == "SO") src = src + " " + l.stripLeading()
            else if (currentCode == "DE") keywords = concat(keywords,getKeywords(l))
            else if (currentCode == "CR") references = getRef(l) :: references
          }
          case _ => currentCode = code
          }
        }
    }
  }
    pubs = Pub(authors,title,src,keywords,references) :: pubs
    count = count+1
    authors = null
    title = null
    src = null
    keywords = null
    references  = List()
  }

  println("Number of records is " + count)
  reverse(pubs).tail foreach {p => {println(p);println("**********")}}

}
