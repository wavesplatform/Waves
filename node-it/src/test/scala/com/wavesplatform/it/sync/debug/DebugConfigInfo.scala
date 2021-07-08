package com.wavesplatform.it.sync.debug

import com.typesafe.config.Config
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.{BaseFunSuite, NodeConfigs}

class DebugConfigInfo extends BaseFunSuite {

  override protected val nodeConfigs: Seq[Config] = NodeConfigs.newBuilder.withDefault(1).build()

  test("getting a configInfo") {
    nodes.head.getWithApiKey(s"/debug/configInfo?full=false")
    nodes.last.getWithApiKey(s"/debug/configInfo?full=true")
  }

}
