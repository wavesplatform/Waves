package com.wavesplatform.report

import io.qase.api.QaseClient
import io.qase.api.config.QaseConfig.RUN_ID_KEY
import io.qase.client.api.RunsApi

object QaseRunCompleter extends App {
  if (QaseClient.getConfig.isEnabled) {
    val apiClient = QaseClient.getApiClient
    new RunsApi(apiClient).completeRun(QaseClient.getConfig.projectCode(), QaseClient.getConfig.runId())
    System.clearProperty(RUN_ID_KEY)
  }
}
