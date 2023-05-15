package com.wavesplatform.report

import io.qase.api.QaseClient
import io.qase.api.config.QaseConfig.{API_TOKEN_KEY, ENABLE_KEY, PROJECT_CODE_KEY, RUN_ID_KEY, RUN_NAME_KEY, USE_BULK_KEY}
import io.qase.client.ApiClient
import io.qase.client.api.RunsApi
import io.qase.client.model.RunCreate

object QaseRunCreator extends App {
  private val CHECKPR_RUN_ID_KEY = "CHECKPR_RUN_ID"

  if (QaseClient.getConfig.isEnabled) {
    val apiClient = new ApiClient()
    for {
      apiToken    <- Option(QaseClient.getConfig.apiToken())
      projectCode <- Option(QaseClient.getConfig.projectCode())
    } yield {
      apiClient.setBasePath(QaseClient.getConfig.baseUrl())
      apiClient.setApiKey(apiToken)
      val description = Option(System.getenv(CHECKPR_RUN_ID_KEY))
        .map(rId => s"[GitHub checkPR action run details](https://github.com/wavesplatform/Waves/actions/runs/$rId)")
        .getOrElse("Local checkPR run")
      val title = Option(QaseClient.getConfig.runName()).getOrElse("unknown")
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
      System.setProperty(RUN_ID_KEY, String.valueOf(runId))
    }
  }
}
