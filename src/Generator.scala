import java.io.{File, IOException, FileWriter}
import java.nio.file.{Paths, Files}

import scala.io.Source
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

/**
 * Created by sema on 10/09/14.
 */
object Generator {

  val ALREADY_PROCESSED:Int = 1
  val NO_PUBLIC_FIELDS:Int = 2
  val ERROR:Int = -1
  val OK:Int = 0

  type CodeLines = List[String]
  val blankLine = List(" ")
  val closingBracket = List( "}" )

  val comparatorMethodMark:String = "public function get comparableFields():Array"


  def apply( file:File ):Int = {


    // get content
    val content: CodeLines = Source.fromFile(file).getLines().toList

    // get public fields
    val publicFields: CodeLines = getFieldsByRegexp(content, "(public var) ([A-Za-z]+)".r)

    
    ( checkIfAlreadyGenerated(content), publicFields.isEmpty ) match {

      case (true, _ ) => ALREADY_PROCESSED
      case ( _, true ) => NO_PUBLIC_FIELDS
      case( _, _ ) => {

        // generate static field references
        val staticReferences: CodeLines = publicFields map makeStaticReference

        // create static method for comparator
        val comparatorMethod: CodeLines = generateComparatorMethod(publicFields)

        try {
          writeFile(file, generateOutput(content, staticReferences, comparatorMethod))
          OK
        }
        catch {

          case e: Exception => ERROR

        }
        
        
      }  
      
    }
    
  }

  @throws(classOf[IOException])
  private def writeFile( file:File, content:CodeLines ) = {

      val writer: FileWriter = new FileWriter( file )
      writer.write(content.mkString("\n"))
      writer.close()

  }

  def checkIfAlreadyGenerated(content:CodeLines):Boolean = content.mkString.contains( comparatorMethodMark )

  private def getFieldsByRegexp( codeLines:CodeLines, regex:Regex ):CodeLines = {


    codeLines.map( regex.findFirstMatchIn( _ ).getOrElse() match {

      case fieldMatch:Match => fieldMatch.group(2)
      case _ => ""

    } ).filter( _.nonEmpty )

  }

  def generateComparatorMethod(fields: CodeLines): CodeLines = {

    val statics:CodeLines = fields.map( (x:String) => camelToUnderscore(x)+",")
    List("public function get comparableFields():Array {", "return [" ) ::: statics ::: List("];", "}")

  }

  def generateOutput( content:CodeLines, statics:CodeLines, comparatorMarker:CodeLines ):CodeLines = {

      content.filter( _.trim.nonEmpty).dropRight(2) :::
      blankLine :::
      statics :::
      blankLine :::
      comparatorMarker :::
      closingBracket :::
      closingBracket
  }

  def makeStaticReference( field:String ):String  =
    "public static const " + camelToUnderscore(field) + ":String = \""+field+"\";"

  val camelToUnderscore: (String) => String = (field:String) => field.map {

    case letter if letter.isUpper => "_" + letter.toUpper
    case x => x.toUpper

  }.mkString

}
