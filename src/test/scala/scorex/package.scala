import java.io.File
import java.util.UUID

package object scorex {
  def createTestTemporaryFolder(): String = {
    val temporaryFolder = System.getProperty("java.io.tmpdir")
    val uuid = UUID.randomUUID().toString
    val folder = s"$temporaryFolder/$uuid/"

    new File(folder).mkdirs()

    folder
  }

}
