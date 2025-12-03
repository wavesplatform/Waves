package com.wavesplatform.transaction.assets

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.protobuf.StaticAssetInfo
import com.wavesplatform.database.{DBExt, Keys}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.v1.traits.domain.Issue
import com.wavesplatform.test.FlatSpec
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.{EthHelpers, JsonMatchers}
import org.scalatest.ParallelTestExecution

class Erc20NoConflictIssueTest extends FlatSpec with EthHelpers with WithDomain with ParallelTestExecution with JsonMatchers {
  "Erc20 should be unique" should "in invoke" in {
    withDomain(DomainPresets.RideV6, Seq.empty) { d =>
      val accountScript = TxHelpers.scriptV6("""
                                               |@Callable(i)
                                               |func test() = {
                                               |  let issue = Issue("test", "test", 1, 1, true)
                                               |  [issue, StringEntry("assetId", toBase58String(calculateAssetId(issue)))]
                                               |}
                                               |""".stripMargin)
      val setScriptTx = TxHelpers.setScript(TxHelpers.defaultSigner, accountScript)
      d.appendBlock(setScriptTx)

      val invokeTx = TxHelpers.invoke(TxHelpers.defaultAddress, Some("test"), fee = 100500000)

      // Calculate assetId before the appending the block, using the same values as in the script
      val calculatedAssetId = Issue.calculateId(
        decimals = 1,
        description = "test",
        isReissuable = true,
        name = "test",
        quantity = 1,
        nonce = 0,
        parent = invokeTx.id()
      )

      // Note: Because reproducing a conflicting assetId is too hard, we create a conflicting assetId manually
      val conflictingAssetId   = ByteStr(calculatedAssetId.arr.take(20) ++ Array.fill[Byte](12)(0))
      val conflictingAssetInfo = StaticAssetInfo() // Note: Use default values for simplicity
      d.rdb.db.readWrite { rw =>
        rw.put(Keys.assetStaticInfo(IssuedAsset(conflictingAssetId)), Some(conflictingAssetInfo))
      }

      d.appendBlockE(invokeTx) should matchPattern {
        case Left(err) if err.toString.contains(s"Asset ${calculatedAssetId} is already issued") =>
      }
    }
  }

  it should "in plain issue tx" in {
    withDomain(DomainPresets.RideV6, Seq.empty) { d =>
      val issueTx         = TxHelpers.issue(issuer = TxHelpers.defaultSigner)
      val assetToBeIssued = issueTx.asset

      // Note: Because reproducing a conflicting assetId is too hard, we create a conflicting assetId manually
      val conflictingAssetId   = ByteStr(assetToBeIssued.id.arr.take(20) ++ Array.fill[Byte](12)(0))
      val conflictingAssetInfo = StaticAssetInfo() // Note: Use default values for simplicity
      d.rdb.db.readWrite { rw =>
        rw.put(Keys.assetStaticInfo(IssuedAsset(conflictingAssetId)), Some(conflictingAssetInfo))
      }

      d.appendBlockE(issueTx) should matchPattern {
        case Left(err) if err.toString.contains(s"Asset ${assetToBeIssued} is already issued") =>
      }
    }
  }
}
