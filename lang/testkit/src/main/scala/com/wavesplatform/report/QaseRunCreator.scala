package com.wavesplatform.report

import com.wavesplatform.report.QaseReporter.{QaseProjects, RunIdKeyPrefix}
import io.qase.api.QaseClient
import io.qase.api.config.QaseConfig.{PROJECT_CODE_KEY, RUN_ID_KEY}
import io.qase.client.ApiClient
import io.qase.client.api.RunsApi
import io.qase.client.model.RunCreate

object QaseRunCreator extends App {
  private val CheckPRRunIdKey = "CHECKPR_RUN_ID"

  if (QaseClient.getConfig.isEnabled) {
    val apiClient = new ApiClient()
    Option(QaseClient.getConfig.apiToken()).foreach { apiToken =>
      apiClient.setBasePath(QaseClient.getConfig.baseUrl())
      apiClient.setApiKey(apiToken)
      val description = Option(System.getenv(CheckPRRunIdKey))
        .map(rId => s"[GitHub checkPR action run details](https://github.com/wavesplatform/Waves/actions/runs/$rId)")
        .getOrElse("Local checkPR run")
      val title = Option(QaseClient.getConfig.runName()).getOrElse("unknown")

      QaseProjects.foreach { projectCode =>
        val runId = new RunsApi(apiClient)
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

        System.setProperty(s"$RunIdKeyPrefix$projectCode", String.valueOf(runId))
      }
    }

    // actually not used but need to be not null for QaseClient initialization
    System.setProperty(RUN_ID_KEY, "1")
    System.setProperty(PROJECT_CODE_KEY, "PR")
  }
}
