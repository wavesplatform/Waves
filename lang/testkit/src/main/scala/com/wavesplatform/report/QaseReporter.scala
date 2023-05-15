package com.wavesplatform.report

import io.qase.api.QaseClient
import io.qase.api.services.impl.ReportersResultOperationsImpl
import io.qase.api.utils.IntegrationUtils
import io.qase.client.ApiClient
import io.qase.client.api.{ResultsApi, RunsApi}
import io.qase.client.model.ResultCreate
import org.scalatest.Reporter
import org.scalatest.events.{Event, RunAborted, RunCompleted, RunStopped, TestFailed, TestSucceeded}

class QaseReporter extends Reporter {

  val apiClient: ApiClient = QaseClient.getApiClient
  val runsApi              = new RunsApi(apiClient)
  val resultOperations     = new ReportersResultOperationsImpl(new ResultsApi(apiClient))

  override def apply(event: Event): Unit = {
    event match {
      case ts: TestSucceeded =>
        extractCaseIds(ts.testName).foreach { caseId =>
          finishTestCase(ResultCreate.StatusEnum.PASSED, ts.testName, caseId, None, None, ts.duration)
        }
      case tf: TestFailed =>
        extractCaseIds(tf.testName).foreach { caseId =>
          finishTestCase(ResultCreate.StatusEnum.FAILED, tf.testName, caseId, tf.throwable, Some(tf.message), tf.duration)
        }
      case _: RunAborted | _: RunStopped | _: RunCompleted =>
        finishRun()
      case _ => ()
    }
  }

  private def extractCaseIds(testName: String): Seq[Long] = {
    val pattern = "NODE-([0-9]+)".r
    pattern.findAllMatchIn(testName).map(_.group(1).toLong).toSeq
  }

  private def finishTestCase(
      status: ResultCreate.StatusEnum,
      testName: String,
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
        this.resultOperations.addBulkResult(resultCreate)
      else
        this.resultOperations.send(resultCreate)
    }

  private def finishRun() =
    if (QaseClient.isEnabled) {
      if (QaseClient.getConfig.useBulk) this.resultOperations.sendBulkResult()
      if (QaseClient.getConfig.runAutocomplete)
        runsApi.completeRun(QaseClient.getConfig.projectCode, QaseClient.getConfig.runId)
    }
}
