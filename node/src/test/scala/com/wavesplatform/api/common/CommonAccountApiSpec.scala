package com.wavesplatform.api.common

import com.wavesplatform.{history, BlocksTransactionsHelpers, TransactionGen}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.traits.domain.{Lease, Recipient}
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.{diffs, DataEntry, Diff, EmptyDataEntry, StringDataEntry}
import com.wavesplatform.transaction.{DataTransaction, GenesisTransaction, TxHelpers}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class CommonAccountApiSpec
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
        data4          = DataTransaction.selfSigned(2.toByte, acc, Seq(EmptyDataEntry("test"), EmptyDataEntry("test1")), fee, ts).explicitGet()
        data5          = DataTransaction.selfSigned(2.toByte, acc, Seq(EmptyDataEntry("test2"), entry1, entry2), fee, ts).explicitGet()
        (block1, mbs1) = UnsafeBlocks.unsafeChainBaseAndMicro(history.randomSig, Seq(genesis), Seq(Seq(data1)), acc, 3, ts)
        (block2, mbs2) = UnsafeBlocks.unsafeChainBaseAndMicro(mbs1.last.totalResBlockSig, Seq(data2), Seq(Seq(data3)), acc, 3, ts)
        (block3, mbs3) = UnsafeBlocks.unsafeChainBaseAndMicro(mbs2.last.totalResBlockSig, Seq(data4), Seq(Seq(data5)), acc, 3, ts)
      } yield (acc, block1, mbs1.head, block2, mbs2.head, block3, mbs3.head)

      forAll(preconditions) {
        case (acc, block1, mb1, block2, mb2, block3, mb3) =>
          withDomain(
            domainSettingsWithFS(
              TestFunctionalitySettings.withFeatures(
                BlockchainFeatures.NG,
                BlockchainFeatures.DataTransaction,
                BlockchainFeatures.BlockV5
              )
            )
          ) { d =>
            val commonAccountsApi             = CommonAccountsApi(d.blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty), d.db, d.blockchainUpdater)
            def dataList(): Set[DataEntry[_]] = commonAccountsApi.dataStream(acc.toAddress, None).toListL.runSyncUnsafe().toSet

            d.appendBlock(block1)
            d.appendMicroBlock(mb1)
            dataList() shouldBe Set(entry1)
            d.appendBlock(block2)
            dataList() shouldBe Set(entry1, entry2)
            d.appendMicroBlock(mb2)
            dataList() shouldBe Set(entry1, entry2, entry3)
            d.appendBlock(block3)
            dataList() shouldBe Set(entry3)
            d.appendMicroBlock(mb3)
            dataList() shouldBe Set(entry1, entry2)
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

  "Waves distribution" - {
    "height is taken into account" in pending
    "balances only present in diff are not ignored" in pending
    "ignores zero balances" in pending
    "diff is ignored when height is less than blockchain height" in pending
    "diff is applied when height matches blockchain height" in pending
  }

  "Asset distribution" - {
    "works for NFT" in pending
    "balances only present in diff are not ignored" in pending
    "ignores zero balances" in pending
    "returns balances only for requested asset" in pending
    "height is taken into account" in pending
    "diff is ignored when height is less than blockchain height" in pending
    "diff is applied when height matches blockchain height" in pending
  }

  "Active leases for address" - {
    "does not return leases which were cancelled in diff" in pending
    "includes new leases from diff" in pending
  }

  "Portfolio" - {
    "includes NFT balances when ReducedNFTFee feature is inactive" in pending
    "excludes NFT balances when ReducedNFTFee feature is active" - {
      "from diff" in pending
      "from leveldb" in pending
    }
  }

  "NFT list" - {
    "does not include NFTs which were spent in diff" in pending
    "includes NFTs which were received in diff" in pending
  }

  "Lease info" - {
    "shows info of lease made through invoke" in withDomain(domainSettingsWithPreactivatedFeatures(BlockchainFeatures.SynchronousCalls, BlockchainFeatures.Ride4DApps)) { d =>
      val dAppScript = TestCompiler(V5).compileContract(
        s"""
           |{-# STDLIB_VERSION 5 #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |{-# CONTENT_TYPE DAPP #-}
           |
           |@Callable(i)
           |func test() = {
           |  [Lease(Address(base58'${TxHelpers.defaultAddress}'), 1, 1)]
           |}
           |""".stripMargin
      )

      val invoke = TxHelpers.invoke(TxHelpers.secondAddress, "test")
      d.appendBlock(
        TxHelpers.genesis(TxHelpers.defaultAddress),
        TxHelpers.genesis(TxHelpers.secondAddress),
        TxHelpers.setScript(TxHelpers.secondSigner, dAppScript),
        invoke
      )

      val api = CommonAccountsApi(Diff.empty, d.db, d.blockchain)
      val leaseId = Lease.calculateId(
        Lease(
          Recipient.Address(ByteStr(TxHelpers.defaultAddress.bytes)),
          1,
          1
        ),
        invoke.id()
      )
      api.leaseInfo(leaseId) shouldBe Some(
        LeaseInfo(leaseId, invoke.id(), TxHelpers.secondAddress, TxHelpers.defaultAddress, 1, 1, LeaseInfo.Status.Active)
      )
    }
  }
}
