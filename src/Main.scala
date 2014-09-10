import java.io.File



/**
 * Created by sema on 10/09/14.
 */
object Main {

  val isDirOrPureASFile:(File) => Boolean = (file:File) => file.isDirectory || file.getName.contains(".as")
  val offsetString = "  "

  def main(args:Array[String]) = {


    val files = args.map( new File(_) ).filter( isDirOrPureASFile )
    for ( file:File <- files) {

      if( !file.isDirectory ) processFile( file ) else processDir(file)

    }

    def processDir( dir:File, indent:Int = 1 ):Unit = {

      println( "processing " + dir.getPath + " -> ...")
      val files = dir.listFiles.filter( isDirOrPureASFile )
      for( file:File <- files ) {

        if( !file.isDirectory ) processFile( file, indent ) else processDir(file, indent +1 )

      }


    }

    def processFile( file:File, indent:Int = 1 ) = {

      val fileName = file.getName
      println( offsetString * indent + (if (!(file.exists && file.canWrite))  fileName + " ... Cannot write file or file does not exists" else {

        Generator(file) match {

          case Generator.OK => fileName + " ... Done."
          case Generator.ALREADY_PROCESSED => fileName + " ... Skipping, already processed"
          case Generator.NO_PUBLIC_FIELDS => fileName + " ... Skipping, no public fields"
          case _ => fileName + " !!! Error"

        }
      }))

    }

  }
}

