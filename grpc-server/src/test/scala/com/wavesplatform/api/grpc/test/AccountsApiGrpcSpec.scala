package com.wavesplatform.api.grpc.test

import com.google.protobuf.ByteString
import com.wavesplatform.TestValues
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.api.grpc.*
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.{DataEntry, Recipient}
import com.wavesplatform.state.{BlockRewardCalculator, EmptyDataEntry, IntegerDataEntry}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.DiffMatchers
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{Assertion, BeforeAndAfterAll}

import scala.concurrent.Await
import scala.concurrent.duration.{DurationInt, FiniteDuration}

class AccountsApiGrpcSpec extends FreeSpec with BeforeAndAfterAll with DiffMatchers with WithDomain with GrpcApiHelpers {

  val sender: KeyPair         = TxHelpers.signer(1)
  val recipient: KeyPair      = TxHelpers.signer(2)
  val timeout: FiniteDuration = 2.minutes

  "GetBalances should work" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
    val grpcApi = getGrpcApi(d)

    val assetTransferAmount   = 123
    val wavesTransferAmount   = 456 + TestValues.fee
    val reverseTransferAmount = 1

    val issue     = TxHelpers.issue(sender)
    val transfer1 = TxHelpers.transfer(sender, recipient.toAddress, assetTransferAmount, issue.asset)
    val transfer2 = TxHelpers.transfer(sender, recipient.toAddress, wavesTransferAmount, Waves)
    val transfer3 = TxHelpers.transfer(recipient, sender.toAddress, reverseTransferAmount, Waves)

    d.appendBlock(issue)
    d.appendBlock(transfer1, transfer2, transfer3)

    d.liquidAndSolidAssert { () =>
      val expectedWavesBalance = wavesTransferAmount - TestValues.fee - reverseTransferAmount
      val expectedResult = List(
        BalanceResponse.of(
          BalanceResponse.Balance.Waves(BalanceResponse.WavesBalances(expectedWavesBalance, 0, expectedWavesBalance, expectedWavesBalance))
        ),
        BalanceResponse.of(BalanceResponse.Balance.Asset(Amount(ByteString.copyFrom(issue.asset.id.arr), assetTransferAmount)))
      )

      val (observer1, result1) = createObserver[BalanceResponse]
      grpcApi.getBalances(
        BalancesRequest.of(ByteString.copyFrom(recipient.toAddress.bytes), Seq(ByteString.EMPTY, ByteString.copyFrom(issue.asset.id.arr))),
        observer1
      )
      result1.runSyncUnsafe() shouldBe expectedResult

      val (observer2, result2) = createObserver[BalanceResponse]
      grpcApi.getBalances(
        BalancesRequest.of(ByteString.copyFrom(recipient.toAddress.bytes), Seq.empty),
        observer2
      )
      result2.runSyncUnsafe() shouldBe expectedResult
    }
  }

  "GetScript should work" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
    val grpcApi = getGrpcApi(d)

    val script = TxHelpers.script(
      s"""{-# STDLIB_VERSION 6 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |@Callable(i)
         |func foo(a: Int) = { ([], a == 42) }""".stripMargin
    )

    d.appendBlock(TxHelpers.setScript(sender, script))

    val r = Await.result(
      grpcApi.getScript(AccountRequest.of(ByteString.copyFrom(sender.toAddress.bytes))),
      timeout
    )

    r.scriptBytes shouldBe ByteString.copyFrom(script.bytes().arr)
    r.publicKey shouldBe ByteString.copyFrom(sender.publicKey.arr)
  }

  "GetActiveLeases should work" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
    val grpcApi = getGrpcApi(d)

    val lease1      = TxHelpers.lease(sender, recipient.toAddress, 123)
    val lease2      = TxHelpers.lease(sender, recipient.toAddress, 456)
    val lease3      = TxHelpers.lease(sender, recipient.toAddress, 789)
    val leaseCancel = TxHelpers.leaseCancel(lease1.id(), sender)

    d.appendBlock(lease1, lease2, lease3)
    d.appendBlock(leaseCancel)

    d.liquidAndSolidAssert { () =>
      val (observer, result) = createObserver[LeaseResponse]

      grpcApi.getActiveLeases(AccountRequest.of(ByteString.copyFrom(recipient.toAddress.bytes)), observer)

      result.runSyncUnsafe() shouldBe List(
        LeaseResponse.of(
          ByteString.copyFrom(lease3.id().arr),
          ByteString.copyFrom(lease3.id().arr),
          ByteString.copyFrom(sender.toAddress.bytes),
          Some(Recipient.of(Recipient.Recipient.PublicKeyHash(ByteString.copyFrom(recipient.toAddress.publicKeyHash)))),
          lease3.amount.value,
          2
        ),
        LeaseResponse.of(
          ByteString.copyFrom(lease2.id().arr),
          ByteString.copyFrom(lease2.id().arr),
          ByteString.copyFrom(sender.toAddress.bytes),
          Some(Recipient.of(Recipient.Recipient.PublicKeyHash(ByteString.copyFrom(recipient.toAddress.publicKeyHash)))),
          lease2.amount.value,
          2
        )
      )
    }
  }

  "GetDataEntries should work" in withDomain(DomainPresets.RideV6, AddrWithBalance.enoughBalances(sender)) { d =>
    val grpcApi = getGrpcApi(d)

    val data       = TxHelpers.dataV2(sender, Seq(IntegerDataEntry("key1", 123), IntegerDataEntry("key2", 456), IntegerDataEntry("key3", 789)))
    val deleteData = TxHelpers.dataV2(sender, Seq(EmptyDataEntry("key1")))

    d.appendBlock(data)
    d.appendBlock(deleteData)

    d.liquidAndSolidAssert { () =>
      val (observer1, result1) = createObserver[DataEntryResponse]
      grpcApi.getDataEntries(DataRequest.of(ByteString.copyFrom(sender.toAddress.bytes), "key2"), observer1)
      result1.runSyncUnsafe() shouldBe List(
        DataEntryResponse.of(
          ByteString.copyFrom(sender.toAddress.bytes),
          Some(DataEntry.of("key2", DataEntry.Value.IntValue(456)))
        )
      )

      val (observer2, result2) = createObserver[DataEntryResponse]
      grpcApi.getDataEntries(DataRequest.of(ByteString.copyFrom(sender.toAddress.bytes), ""), observer2)
      result2.runSyncUnsafe() shouldBe List(
        DataEntryResponse.of(
          ByteString.copyFrom(sender.toAddress.bytes),
          Some(DataEntry.of("key2", DataEntry.Value.IntValue(456)))
        ),
        DataEntryResponse.of(
          ByteString.copyFrom(sender.toAddress.bytes),
          Some(DataEntry.of("key3", DataEntry.Value.IntValue(789)))
        )
      )
    }
  }

  "NODE-922. GetBalances should return correct balances for challenged and challenging miners" in {
    def checkBalances(
        address: Address,
        expectedRegular: Long,
        expectedEffective: Long,
        expectedGenerating: Long,
        grpcApi: AccountsApiGrpcImpl
    ): Assertion = {
      val expectedResult = List(
        BalanceResponse.of(
          BalanceResponse.Balance.Waves(BalanceResponse.WavesBalances(expectedRegular, expectedGenerating, expectedRegular, expectedEffective))
        )
      )

      val (observer, result) = createObserver[BalanceResponse]
      grpcApi.getBalances(
        BalancesRequest.of(ByteString.copyFrom(address.bytes), Seq.empty),
        observer
      )
      result.runSyncUnsafe() shouldBe expectedResult
    }

    val sender          = TxHelpers.signer(1)
    val challengedMiner = TxHelpers.signer(2)
    withDomain(DomainPresets.TransactionStateSnapshot, balances = AddrWithBalance.enoughBalances(sender)) { d =>
      val grpcApi = getGrpcApi(d)

      val challengingMiner = d.wallet.generateNewAccount().get

      val initChallengingBalance = 1000.waves
      val initChallengedBalance  = 2000.waves

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, initChallengingBalance),
        TxHelpers.transfer(sender, challengedMiner.toAddress, initChallengedBalance)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val invalidStateHash = ByteStr.fill(DigestLength)(1)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq.empty,
        strictTime = true,
        generator = challengedMiner,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = d.createChallengingBlock(challengingMiner, originalBlock)

      checkBalances(challengingMiner.toAddress, initChallengingBalance, initChallengingBalance, initChallengingBalance, grpcApi)
      checkBalances(challengedMiner.toAddress, initChallengedBalance, initChallengedBalance, initChallengedBalance, grpcApi)

      d.appendBlockE(challengingBlock) should beRight

      checkBalances(
        challengingMiner.toAddress,
        initChallengingBalance + getLastBlockMinerReward(d),
        initChallengingBalance + getLastBlockMinerReward(d),
        initChallengingBalance,
        grpcApi
      )
      checkBalances(challengedMiner.toAddress, initChallengedBalance, 0, 0, grpcApi)

      d.appendBlock()

      checkBalances(
        challengingMiner.toAddress,
        initChallengingBalance + getLastBlockMinerReward(d),
        initChallengingBalance + getLastBlockMinerReward(d),
        initChallengingBalance,
        grpcApi
      )
      checkBalances(challengedMiner.toAddress, initChallengedBalance, initChallengedBalance, 0, grpcApi)

    }
  }

  private def getGrpcApi(d: Domain) =
    new AccountsApiGrpcImpl(d.accountsApi)

  private def getLastBlockMinerReward(d: Domain): Long =
    BlockRewardCalculator
      .getBlockRewardShares(
        d.blockchain.height,
        d.blockchain.settings.rewardsSettings.initial,
        d.blockchain.settings.functionalitySettings.daoAddressParsed.toOption.flatten,
        d.blockchain.settings.functionalitySettings.daoAddressParsed.toOption.flatten,
        d.blockchain
      )
      .miner
}
