package com.wavesplatform.report

import cats.syntax.functor.*
import com.wavesplatform.report.QaseReporter.{QaseProjects, getProjectToRunIds}
import io.qase.api.QaseClient
import io.qase.api.exceptions.QaseException
import io.qase.api.services.impl.ReportersResultOperationsImpl
import io.qase.api.utils.IntegrationUtils
import io.qase.client.ApiClient
import io.qase.client.api.{ResultsApi, RunsApi}
import io.qase.client.model.ResultCreate
import org.scalatest.Reporter
import org.scalatest.events.{Event, RunAborted, RunCompleted, RunStopped, TestFailed, TestSucceeded}
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

class QaseReporter extends Reporter {

  val logger: Logger                       = LoggerFactory.getLogger(this.getClass)
  val projectToRunIds: Map[String, String] = getProjectToRunIds
  val apiClient: ApiClient                 = QaseClient.getApiClient
  val runsApi                              = new RunsApi(apiClient)
  val resultsApi                           = new ResultsApi(apiClient)
  val resultOperations: Map[String, ReportersResultOperationsImpl] =
    QaseProjects.map(projectCode => projectCode -> new ReportersResultOperationsImpl(resultsApi)).toMap

  override def apply(event: Event): Unit = {
    event match {
      case ts: TestSucceeded =>
        extractCaseIds(ts.testName).foreach { case (projectCode, caseId) =>
          finishTestCase(ResultCreate.StatusEnum.PASSED, ts.testName, projectCode, caseId, None, None, ts.duration)
        }
      case tf: TestFailed =>
        extractCaseIds(tf.testName).foreach { case (projectCode, caseId) =>
          finishTestCase(ResultCreate.StatusEnum.FAILED, tf.testName, projectCode, caseId, tf.throwable, Some(tf.message), tf.duration)
        }
      case _: RunAborted | _: RunStopped | _: RunCompleted =>
        finishRun()
      case _ => ()
    }
  }

  private def extractCaseIds(testName: String): Seq[(String, Long)] = {
    val patternStr = s"""(${QaseProjects.mkString("|")})-([0-9]+)"""
    val pattern    = patternStr.r

    pattern.findAllMatchIn(testName).map(m => m.group(1) -> m.group(2).toLong).toSeq
  }

  private def finishTestCase(
      status: ResultCreate.StatusEnum,
      testName: String,
      projectCode: String,
      caseId: Long,
      throwable: Option[Throwable],
      msgOpt: Option[String],
      duration: Option[Long]
  ): Unit =
    if (QaseClient.isEnabled) {
      val resultCreate = new ResultCreate()
      val errMsg       = msgOpt.map(msg => s"\n\n**Error**\n$msg").getOrElse("")
      val comment      = s"$testName$errMsg"
      val stacktrace   = throwable.map(IntegrationUtils.getStacktrace).orNull
      val timeMs       = duration.getOrElse(0L)
      resultCreate
        .status(status)
        .comment(comment)
        .stacktrace(stacktrace)
        .caseId(caseId)
        .timeMs(timeMs)

      if (QaseClient.getConfig.useBulk)
        this.resultOperations.get(projectCode).foreach(_.addBulkResult(resultCreate))
      else {
        projectToRunIds.get(projectCode).foreach { runId =>
          Try(resultsApi.createResult(projectCode, runId.toInt, resultCreate)).void.recover { case qe: QaseException =>
            logger.warn("Error while sending test result occurred", qe)
          }.get
        }
      }
    }

  private def finishRun(): Unit =
    if (QaseClient.isEnabled) {
      if (QaseClient.getConfig.useBulk) {
        QaseProjects.foreach { projectCode =>
          this.resultOperations.get(projectCode).foreach(_.sendBulkResult())
        }
      }
      if (QaseClient.getConfig.runAutocomplete) {
        projectToRunIds.foreach { case (projectCode, runId) =>
          runsApi.completeRun(projectCode, runId.toInt)
        }
      }
    }
}

object QaseReporter {
  val RunIdKeyPrefix = "QASE_RUN_ID_"
  val QaseProjects   = Seq("NODE", "RIDE", "BU", "SAPI")

  def getProjectToRunIds: Map[String, String] =
    QaseProjects.flatMap { projectCode =>
      Option(System.getProperty(s"$RunIdKeyPrefix$projectCode")).map(projectCode -> _)
    }.toMap
}
