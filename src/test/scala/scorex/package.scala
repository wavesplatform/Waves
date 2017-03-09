import java.io.File
import java.nio.file.{Files, Path}

package object scorex {
  def createTestTemporaryFolder(): Path = {
    val path = Files.createTempDirectory("waves-test-")
    path.toFile.deleteOnExit()

    path
  }

  def createTestTemporaryFile(name: String, ext: String): File = {
    val file = Files.createTempFile(createTestTemporaryFolder(), name, ext).toFile
    file.deleteOnExit()

    file
  }
}
