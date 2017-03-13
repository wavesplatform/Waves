package com.wavesplatform.it

import org.scalatest.{BeforeAndAfterAllConfigMap, ConfigMap, FreeSpec}

class FirstSpec extends FreeSpec with BeforeAndAfterAllConfigMap {

  "zzz" - {
    "works" in {

    }
  }

  override protected def beforeAll(configMap: ConfigMap): Unit = {
    println(configMap.get("dockerImageId"))
  }
}
