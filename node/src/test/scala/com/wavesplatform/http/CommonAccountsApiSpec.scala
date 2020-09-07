package com.wavesplatform.http

import com.wavesplatform.api.common.CommonAccountsApi
import com.wavesplatform.common.utils._
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.{DataEntry, Diff, StringDataEntry, diffs}
import com.wavesplatform.transaction.{DataTransaction, GenesisTransaction}
import com.wavesplatform.{BlocksTransactionsHelpers, TransactionGen, history}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class CommonAccountsApiSpec
    extends FreeSpec
    with Matchers
    with WithDomain
    with TransactionGen
    with BlocksTransactionsHelpers
    with ScalaCheckDrivenPropertyChecks {
  "Data stream" - {
    "handles non-existent address" in {
      val entry1 = StringDataEntry("test", "test")
      val entry2 = StringDataEntry("test1", "test")
      val entry3 = StringDataEntry("test2", "test")

      val preconditions = for {
        acc <- accountGen
        ts = System.currentTimeMillis()
        fee <- smallFeeGen
        genesis        = GenesisTransaction.create(acc.toAddress, diffs.ENOUGH_AMT, ts).explicitGet()
        data1          = DataTransaction.selfSigned(1.toByte, acc, Seq(entry1), fee, ts).explicitGet()
        data2          = DataTransaction.selfSigned(1.toByte, acc, Seq(entry2), fee, ts).explicitGet()
        data3          = DataTransaction.selfSigned(1.toByte, acc, Seq(entry3), fee, ts).explicitGet()
        (block1, mbs1) = UnsafeBlocks.unsafeChainBaseAndMicro(history.randomSig, Seq(genesis), Seq(Seq(data1)), acc, 3, ts)
        (block2, mbs2) = UnsafeBlocks.unsafeChainBaseAndMicro(mbs1.last.totalResBlockSig, Seq(data2), Seq(Seq(data3)), acc, 3, ts)
      } yield (acc, block1, mbs1.head, block2, mbs2.head)

      forAll(preconditions) {
        case (acc, block1, mb1, block2, mb2) =>
          withDomain(domainSettingsWithFS(TestFunctionalitySettings.withFeatures(BlockchainFeatures.NG, BlockchainFeatures.DataTransaction))) { d =>
            val commonAccountsApi             = CommonAccountsApi(d.blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty), d.db, d.blockchainUpdater)
            def dataList(): Set[DataEntry[_]] = commonAccountsApi.dataStream(acc.toAddress, None).toListL.runSyncUnsafe().toSet

            d.appendBlock(block1)
            dataList() shouldBe empty
            d.appendMicroBlock(mb1)
            dataList() shouldBe Set(entry1)
            d.appendBlock(block2)
            dataList() shouldBe Set(entry1, entry2)
            d.appendMicroBlock(mb2)
            dataList() shouldBe Set(entry1, entry2, entry3)
          }
      }
    }

    "filters liquid entities" in {
      val entry1 = StringDataEntry("test_1", "test")
      val entry2 = StringDataEntry("test-", "test")
      val entry3 = StringDataEntry("test_2", "test")

      val preconditions = for {
        acc <- accountGen
        ts = System.currentTimeMillis()
        fee <- smallFeeGen
        genesis        = GenesisTransaction.create(acc.toAddress, diffs.ENOUGH_AMT, ts).explicitGet()
        data1          = DataTransaction.selfSigned(1.toByte, acc, Seq(entry1), fee, ts).explicitGet()
        data2          = DataTransaction.selfSigned(1.toByte, acc, Seq(entry2), fee, ts).explicitGet()
        data3          = DataTransaction.selfSigned(1.toByte, acc, Seq(entry3), fee, ts).explicitGet()
        (block1, mbs1) = UnsafeBlocks.unsafeChainBaseAndMicro(history.randomSig, Seq(genesis), Seq(Seq(data1)), acc, 3, ts)
        (block2, mbs2) = UnsafeBlocks.unsafeChainBaseAndMicro(mbs1.last.totalResBlockSig, Seq(data2), Seq(Seq(data3)), acc, 3, ts)
      } yield (acc, block1, mbs1.head, block2, mbs2.head)

      forAll(preconditions) {
        case (acc, block1, mb1, block2, mb2) =>
          withDomain(domainSettingsWithFS(TestFunctionalitySettings.withFeatures(BlockchainFeatures.NG, BlockchainFeatures.DataTransaction))) { d =>
            val commonAccountsApi             = CommonAccountsApi(d.blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty), d.db, d.blockchainUpdater)
            def dataList(): Set[DataEntry[_]] = commonAccountsApi.dataStream(acc.toAddress, Some("test_.*")).toListL.runSyncUnsafe().toSet

            d.appendBlock(block1)
            dataList() shouldBe empty
            d.appendMicroBlock(mb1)
            dataList() shouldBe Set(entry1)
            d.appendBlock(block2)
            dataList() shouldBe Set(entry1)
            d.appendMicroBlock(mb2)
            dataList() shouldBe Set(entry1, entry3)
          }
      }
    }
  }
}
