package com.wavesplatform.it.sync.debug

import com.typesafe.config.Config
import com.wavesplatform.it.{NodeConfigs, NodesFromDocker}
import com.wavesplatform.it.api.SyncHttpApi._
import org.scalatest.FunSuite

class DebugConfigInfo extends FunSuite with NodesFromDocker {

  override protected val nodeConfigs: Seq[Config] = NodeConfigs.newBuilder.withDefault(1).build()

  test("getting a configInfo") {
    nodes.head.getWithApiKey(s"/debug/configInfo?full=false")
    nodes.last.getWithApiKey(s"/debug/configInfo?full=true")
  }

}
