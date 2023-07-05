package com.wavesplatform.report

import com.wavesplatform.report.QaseReporter.{CaseIdPattern, QaseProjects, TestResult}
import io.qase.api.QaseClient
import io.qase.api.utils.IntegrationUtils
import io.qase.client.model.ResultCreate
import org.scalatest.Reporter
import org.scalatest.events.*
import play.api.libs.json.{Format, Json}

import java.io.FileWriter
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex

class QaseReporter extends Reporter {

  private val results = initResults()

  override def apply(event: Event): Unit = {
    event match {
      case ts: TestSucceeded =>
        extractCaseIds(ts.testName).foreach { case (projectCode, caseId) =>
          saveTestCaseResults(ResultCreate.StatusEnum.PASSED, ts.testName, projectCode, caseId, None, None, ts.duration)
        }
      case tf: TestFailed =>
        extractCaseIds(tf.testName).foreach { case (projectCode, caseId) =>
          saveTestCaseResults(ResultCreate.StatusEnum.FAILED, tf.testName, projectCode, caseId, tf.throwable, Some(tf.message), tf.duration)
        }
      case _: RunAborted | _: RunStopped | _: RunCompleted =>
        saveRunResults()
      case _ => ()
    }
  }

  private def extractCaseIds(testName: String): Seq[(String, Long)] =
    CaseIdPattern.findAllMatchIn(testName).map(m => m.group(1) -> m.group(2).toLong).toSeq

  private def saveTestCaseResults(
      status: ResultCreate.StatusEnum,
      testName: String,
      projectCode: String,
      caseId: Long,
      throwable: Option[Throwable],
      msgOpt: Option[String],
      duration: Option[Long]
  ): Unit =
    if (QaseClient.isEnabled) {
      val errMsg     = msgOpt.map(msg => s"\n\n**Error**\n$msg").getOrElse("")
      val comment    = s"$testName$errMsg"
      val stacktrace = throwable.map(IntegrationUtils.getStacktrace)
      val timeMs     = duration.getOrElse(0L)

      results.computeIfPresent(projectCode, (_, results) => TestResult(status.toString, comment, stacktrace, caseId, timeMs) +: results)
    }

  private def saveRunResults(): Unit =
    if (QaseClient.isEnabled) {
      results.asScala.foreach { case (projectCode, results) =>
        if (results.nonEmpty) {
          val writer = new FileWriter(s"./$projectCode-${System.currentTimeMillis()}")
          writer.write(Json.toJson(results).toString)
          writer.close()
        }
      }
    }

  private def initResults() = {
    val results = new ConcurrentHashMap[String, List[TestResult]]()
    QaseProjects.foreach(projectCode => results.put(projectCode, List.empty))
    results
  }
}

object QaseReporter {
  val RunIdKeyPrefix  = "QASE_RUN_ID_"
  val CheckPRRunIdKey = "CHECKPR_RUN_ID"
  val QaseProjects    = Seq("NODE", "RIDE", "BU", "SAPI")

  private val patternStr   = s"""(${QaseProjects.mkString("|")})-([0-9]+)"""
  val CaseIdPattern: Regex = patternStr.r

  case class TestResult(status: String, comment: String, stackTrace: Option[String], caseId: Long, timeMs: Long)
  object TestResult {
    implicit val format: Format[TestResult] = Json.format
  }
}
