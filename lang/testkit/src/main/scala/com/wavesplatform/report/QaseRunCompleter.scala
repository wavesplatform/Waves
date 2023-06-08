package com.wavesplatform.report

import io.qase.api.QaseClient
import io.qase.client.api.RunsApi

object QaseRunCompleter extends App {
  if (QaseClient.getConfig.isEnabled) {
    val runIds    = QaseReporter.getProjectToRunIds
    val apiClient = QaseClient.getApiClient
    val runsApi   = new RunsApi(apiClient)

    runIds.foreach { case (projectCode, runId) =>
      runsApi.completeRun(projectCode, runId.toInt)
    }
  }
}
