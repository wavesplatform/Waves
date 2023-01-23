package com.wavesplatform.api.common

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.traits.domain.{Lease, Recipient}
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.{DataEntry, Diff, EmptyDataEntry, StringDataEntry, diffs}
import com.wavesplatform.test.DomainPresets.RideV4
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.TxHelpers.data
import com.wavesplatform.transaction.TxVersion.V2
import com.wavesplatform.transaction.{DataTransaction, GenesisTransaction, TxHelpers}
import com.wavesplatform.{BlocksTransactionsHelpers, history}
import monix.execution.Scheduler.Implicits.global

class CommonAccountApiSpec extends FreeSpec with WithDomain with BlocksTransactionsHelpers {

  "Data stream" - {
    "handles non-existent address" in {
      val entry1 = StringDataEntry("test", "test")
      val entry2 = StringDataEntry("test1", "test")
      val entry3 = StringDataEntry("test2", "test")

      val acc     = accountGen.sample.get
      val genesis = TxHelpers.genesis(acc.toAddress)
      val data1   = data(acc, Seq(entry1))
      val data2   = data(acc, Seq(entry2))
      val data3   = data(acc, Seq(entry3))
      val data4   = data(acc, Seq(EmptyDataEntry("test"), EmptyDataEntry("test1")), version = V2)
      val data5   = data(acc, Seq(EmptyDataEntry("test2"), entry1, entry2), version = V2)

      withDomain(RideV4) { d =>
        val commonAccountsApi = CommonAccountsApi(() => d.blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty), d.db, d.blockchainUpdater)
        def dataList(): Set[DataEntry[_]] = commonAccountsApi.dataStream(acc.toAddress, None).toListL.runSyncUnsafe().toSet

        d.appendBlock(genesis)
        d.appendMicroBlock(data1)
        dataList() shouldBe Set(entry1)
        d.appendBlock(data2)
        dataList() shouldBe Set(entry1, entry2)
        d.appendMicroBlock(data3)
        dataList() shouldBe Set(entry1, entry2, entry3)
        d.appendBlock(data4)
        dataList() shouldBe Set(entry3)
        d.appendMicroBlock(data5)
        dataList() shouldBe Set(entry1, entry2)
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

      forAll(preconditions) { case (acc, block1, mb1, block2, mb2) =>
        withDomain(domainSettingsWithFS(TestFunctionalitySettings.withFeatures(BlockchainFeatures.NG, BlockchainFeatures.DataTransaction))) { d =>
          val commonAccountsApi = CommonAccountsApi(() => d.blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty), d.db, d.blockchainUpdater)
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
    "shows info of lease made through invoke" in
      withDomain(DomainPresets.RideV5, AddrWithBalance.enoughBalances(TxHelpers.defaultSigner, TxHelpers.secondSigner)) { d =>
        val dAppScript = TestCompiler(V5).compileContract(
          s"""
             |{-# STDLIB_VERSION 5 #-}
             |{-# SCRIPT_TYPE ACCOUNT #-}
             |{-# CONTENT_TYPE DAPP #-}
             |
             |@Callable(i)
             |func default() = {
             |  [Lease(Address(base58'${TxHelpers.defaultAddress}'), 1, 1)]
             |}
             |""".stripMargin
        )

        val invoke = TxHelpers.invoke(TxHelpers.secondAddress)
        d.appendBlock(
          TxHelpers.setScript(TxHelpers.secondSigner, dAppScript),
          invoke
        )

        val api = CommonAccountsApi(() => Diff.empty, d.db, d.blockchain)
        val leaseId = Lease.calculateId(
          Lease(
            Recipient.Address(ByteStr(TxHelpers.defaultAddress.bytes)),
            1,
            1
          ),
          invoke.id()
        )
        api.leaseInfo(leaseId) shouldBe Some(
          LeaseInfo(leaseId, invoke.id(), TxHelpers.secondAddress, TxHelpers.defaultAddress, 1, 2, LeaseInfo.Status.Active)
        )
      }
  }
}
