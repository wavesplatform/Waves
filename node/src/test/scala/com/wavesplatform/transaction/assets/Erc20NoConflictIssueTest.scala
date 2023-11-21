package com.wavesplatform.transaction.assets

import com.wavesplatform.lang.v1.traits.domain.Issue
import com.wavesplatform.state.diffs.produceRejectOrFailedDiff
import com.wavesplatform.test.{FreeSpec, SharedDomain}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.TxHelpers.defaultSigner
import org.scalatest.matchers.should.Matchers

class Erc20NoConflictIssueTest extends FreeSpec with Matchers with SharedDomain {
  "Erc20 should be unique" - {
    "in invoke" in {
      val tx      = TxHelpers.invoke(TxHelpers.defaultAddress, Some("test"), fee = 100900000)
      val assetId = IssuedAsset(Issue.calculateId(1, "test", isReissuable = true, "test", 1, 1, tx.id()))

      domain.appendBlock(
        TxHelpers.setScript(
          defaultSigner,
          TxHelpers.scriptV5("""
                               |@Callable(i)
                               |func test() = {
                               |  [Issue("test", "test", 1, 1, true, unit, 1)]
                               |}
                               |""".stripMargin)
        )
      )
      domain.appendBlockE(tx) should produceRejectOrFailedDiff(s"Asset $assetId is already issued")
    }

    "in plain issue tx" in {
      val tx = TxHelpers.issue()
      domain.appendBlockE(tx) should produceRejectOrFailedDiff(s"Asset ${tx.asset} is already issued")
    }
  }
}
