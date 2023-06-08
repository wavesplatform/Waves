package com.wavesplatform.report

import com.wavesplatform.report.QaseReporter.{CheckPRRunIdKey, QaseProjects, TestResult}
import io.qase.api.QaseClient
import io.qase.api.config.QaseConfig.{PROJECT_CODE_KEY, RUN_ID_KEY}
import io.qase.api.services.impl.ReportersResultOperationsImpl
import io.qase.client.ApiClient
import io.qase.client.api.{CasesApi, ResultsApi, RunsApi}
import io.qase.client.model.{GetCasesFiltersParameter, GetRunsFiltersParameter, ResultCreate, RunCreate}
import play.api.libs.json.Json

import java.io.File
import java.nio.file.Files
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object QaseRunCompleter extends App {
  if (QaseClient.getConfig.isEnabled) {

    val apiClient: ApiClient = QaseClient.getApiClient
    val runsApi              = new RunsApi(apiClient)
    val resultsApi           = new ResultsApi(apiClient)
    val casesApi             = new CasesApi(apiClient)

    QaseProjects.foreach { projectCode =>
      waitForActiveRunComplete(runsApi)

      val dir = new File("./")
      val resultFiles = dir.listFiles
        .filter(_.isFile)
        .filter(_.getName.startsWith(projectCode))
        .toSeq

      Using.resource(resultFiles) { resultFiles =>
        val hasCases = casesApi.getCases(projectCode, new GetCasesFiltersParameter(), 1, 0).getResult.getCount > 0

        if (hasCases) {
          val results = resultFiles.flatMap { file =>
            Using.resource(Source.fromFile(file)) { source =>
              Json.parse(source.getLines().mkString("\n")).as[Seq[TestResult]]
            }
          }

          val description = Option(System.getenv(CheckPRRunIdKey))
            .map(rId => s"[GitHub checkPR action run details](https://github.com/wavesplatform/Waves/actions/runs/$rId)")
            .getOrElse("Local checkPR run")
          val title = Option(QaseClient.getConfig.runName()).getOrElse("unknown")

          val runId = runsApi
            .createRun(
              projectCode,
              new RunCreate()
                .title(title)
                .description(description)
                .includeAllCases(true)
                .isAutotest(true)
            )
            .getResult
            .getId
          val resultOps = new ReportersResultOperationsImpl(resultsApi)
          results.foreach { result =>
            resultOps.addBulkResult(
              new ResultCreate()
                .status(ResultCreate.StatusEnum.fromValue(result.status))
                .comment(result.comment)
                .stacktrace(result.stackTrace.orNull)
                .caseId(result.caseId)
                .timeMs(result.timeMs)
            )
          }

          QaseClient.getConfig.setProperty(RUN_ID_KEY, runId.toString)
          QaseClient.getConfig.setProperty(PROJECT_CODE_KEY, projectCode)
          resultOps.sendBulkResult()

          runsApi.completeRun(projectCode, runId.toInt)
        }
      }(_.foreach(f => Files.delete(f.toPath)))
    }
  }

  @tailrec
  private def waitForActiveRunComplete(runsApi: RunsApi, retries: Int = 10): Unit = {
    val hasActiveRuns = QaseProjects.exists { projectCode =>
      runsApi.getRuns(projectCode, new GetRunsFiltersParameter().status("active"), 1, 0, "cases").getResult.getCount > 0
    }
    if (hasActiveRuns && retries > 0) {
      Thread.sleep(3000)
      waitForActiveRunComplete(runsApi, retries - 1)
    }
  }
}
