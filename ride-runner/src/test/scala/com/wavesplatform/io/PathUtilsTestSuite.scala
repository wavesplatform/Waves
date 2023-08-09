package com.wavesplatform.io

import com.google.common.jimfs.{Configuration, Jimfs}
import com.wavesplatform.BaseTestSuite
import org.scalatest.BeforeAndAfterAll
import org.scalatest.prop.TableDrivenPropertyChecks

import java.nio.file.{FileSystem, Path}
import scala.jdk.CollectionConverters.IterableHasAsScala

class PathUtilsTestSuite extends BaseTestSuite with TableDrivenPropertyChecks with BeforeAndAfterAll {
  private val windows = Jimfs.newFileSystem(Configuration.windows())
  private val macOs   = Jimfs.newFileSystem(Configuration.osX())
  private val linux   = Jimfs.newFileSystem(Configuration.unix())
  private val systems = List(windows, macOs, linux)

  private def commonTests(fs: FileSystem): Seq[(Path, Path, Path)] = {
    val rootDir      = fs.getRootDirectories.asScala.headOption.getOrElse(throw new RuntimeException("Can't find a root directory"))
    val fooBarDir    = rootDir.resolve(fs.getPath("foo", "bar"))
    val fooBarBazDir = fooBarDir.resolve("baz")
    Seq(
      (fooBarBazDir, fooBarDir, fooBarDir),
      (fooBarBazDir, fooBarDir.resolve("baz2"), fooBarDir),
      (fooBarBazDir, rootDir.resolve("foo2"), rootDir)
    )
  }

  private val table = Table[Path, Path, Path](
    ("path1", "path2", "expected"),
    (systems.flatMap(commonTests) ++ Seq(
      (windows.getPath("C:\\"), windows.getPath("D:\\"), windows.getPath(""))
    ))*
  )

  "commonPath" in forAll(table) { case (path1, path2, expected) =>
    PathUtils.commonPath(path1, path2) shouldBe expected
  }

  override protected def afterAll(): Unit = {
    windows.close()
    macOs.close()
    linux.close()
    super.afterAll()
  }
}
