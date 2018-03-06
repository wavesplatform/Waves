package com.wavesplatform.state2.diffs.smart.performance

import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

class SigVerifyPerformanceTest  extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  property("1 thread signature check") {

  }

  property("parallel signature check") {

  }

  property("checking signature via sigVerify") {

  }
}
