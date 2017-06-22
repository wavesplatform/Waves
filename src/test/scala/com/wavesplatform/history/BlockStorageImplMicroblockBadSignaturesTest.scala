package com.wavesplatform.history

import com.wavesplatform.TransactionGen
import org.scalacheck.Shrink
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class BlockStorageImplMicroblockBadSignaturesTest extends PropSpec with PropertyChecks with DomainScenarioDrivenPropertyCheck with Matchers with TransactionGen {
  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  ignore("bad microBlock signature") {}
  ignore("bad total resulting block signature") {}
  ignore("other sender") {}
}
