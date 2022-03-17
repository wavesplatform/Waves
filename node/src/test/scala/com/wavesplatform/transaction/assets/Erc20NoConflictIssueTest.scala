package com.wavesplatform.transaction.assets

import com.wavesplatform.test.FreeSpec
import com.wavesplatform.BlockchainStubHelpers
import com.wavesplatform.lang.v1.traits.domain.Issue
import com.wavesplatform.state.diffs.produceRejectOrFailedDiff
import com.wavesplatform.transaction.{ERC20Address, TxHelpers}
import com.wavesplatform.transaction.Asset.IssuedAsset
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.matchers.should.Matchers

class Erc20NoConflictIssueTest extends FreeSpec with Matchers with BlockchainStubHelpers with PathMockFactory {
  "Erc20 should be unique" - {
    "in invoke" in {
      val tx      = TxHelpers.invoke(TxHelpers.defaultAddress, Some("test"), fee = 100900000)
      val assetId = IssuedAsset(Issue.calculateId(1, "test", isReissuable = true, "test", 1, 1, tx.id()))

      val blockchain = createBlockchainStub { b =>
        (b.resolveERC20Address _).when(ERC20Address(assetId)).returns(Some(assetId)) // Only erc20 entry in the blockchain
        b.stub.setScript(
          TxHelpers.defaultAddress,
          TxHelpers.scriptV5("""
            |@Callable(i)
            |func test() = {
            |  [Issue("test", "test", 1, 1, true, unit, 1)]
            |}
            |""".stripMargin)
        )
      }
      val differ = blockchain.stub.transactionDiffer().andThen(_.resultE)
      differ(tx) should produceRejectOrFailedDiff(s"Asset $assetId is already issued")
    }

    "in plain issue tx" in {
      val tx = TxHelpers.issue()
      val blockchain = createBlockchainStub { b =>
        (b.resolveERC20Address _).when(ERC20Address(tx.asset)).returns(Some(tx.asset)) // Only erc20 entry in the blockchain
      }
      val differ = blockchain.stub.transactionDiffer().andThen(_.resultE)
      differ(tx) should produceRejectOrFailedDiff(s"Asset ${tx.asset} is already issued")
    }
  }
}
