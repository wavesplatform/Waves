package com.wavesplatform.jvm

import cats.syntax.option.*
import com.sun.management.HotSpotDiagnosticMXBean
import com.wavesplatform.utils.ScorexLogging
import play.api.libs.json.{Json, OFormat}

import java.lang.management.ManagementFactory
import java.nio.file.{Files, Path}
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}
import java.util.concurrent.TimeUnit
import scala.jdk.StreamConverters.StreamHasToScala

// Temporary
object HeapDumps extends ScorexLogging {
  private val enabled = Option(System.getProperty("ride.heapDumps.enabled")).nonEmpty

  private val dateFormatter = DateTimeFormatter
    .ofPattern("yyyy-MM-dd_HH_mm")

  private val baseDir   = Path.of("/var", "lib", "ride-runner", "heap-dumps")
  private val onExitDir = baseDir.resolve("on-exit")
  if (enabled) {
    log.info("Heap dumps enabled")
    Files.createDirectories(onExitDir)
  }

  def cleanDirs(retainFiles: Int): Unit = if (enabled) {
    cleanDir(baseDir, retainFiles)
    cleanDir(onExitDir, retainFiles)
  }

  private def cleanDir(path: Path, retainFiles: Int): Unit =
    Files
      .list(path)
      .toScala(List)
      .filterNot(_ == onExitDir)
      .sortBy(x => Files.getLastModifiedTime(x).to(TimeUnit.MILLISECONDS))
      .drop(retainFiles)
      .foreach(Files.delete)

  def all(): List[HeapDump] =
    java.util.stream.Stream
      .concat(
        Files.list(baseDir),
        Files.list(onExitDir)
      )
      .toScala(List)
      .filterNot(_ == onExitDir)
      .sortBy(_.getFileName)
      .map { x =>
        HeapDump(
          relativePath = baseDir.relativize(x).toString,
          size = Files.size(x)
        )
      }

  def pathToFile(relativePath: String): Option[Path] = {
    val resolved = baseDir.resolve(relativePath).normalize()
    if (resolved.startsWith(baseDir)) resolved.some
    else none
  }

  /** @param makeAlways
    *   If true makes only if the directory is empty
    */
  def mk(tag: String, live: Boolean, makeAlways: Boolean = true): Option[String] = if (enabled) {
    // TODO settings?
    val date = dateFormatter.format(Instant.now().atZone(ZoneId.of("UTC")))
    val file = baseDir.resolve(s"ride-runner-$tag-$date.hprof")

    if (makeAlways || Files.list(baseDir).findFirst().isEmpty) {
      val server = ManagementFactory.getPlatformMBeanServer
      val mxBean = ManagementFactory.newPlatformMXBeanProxy(
        server,
        "com.sun.management:type=HotSpotDiagnostic",
        classOf[HotSpotDiagnosticMXBean]
      )
      log.info(s"Making a heap dump $file")
      mxBean.dumpHeap(file.toString, live)
    }

    file.getFileName.toString.some
  } else none
}

case class HeapDump(relativePath: String, size: Long)
object HeapDump {
  implicit val heapDumpFormat: OFormat[HeapDump] = Json.format
}
