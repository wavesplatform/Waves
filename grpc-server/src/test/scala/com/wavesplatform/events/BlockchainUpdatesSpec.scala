package com.wavesplatform.events

import com.google.common.primitives.Longs
import com.google.protobuf.ByteString
import com.wavesplatform.TestValues
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.utils.*
import com.wavesplatform.db.InterferableDB
import com.wavesplatform.events.FakeObserver.*
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
import com.wavesplatform.events.StateUpdate.{AssetInfo, AssetStateUpdate, BalanceUpdate, DataEntryUpdate, LeaseUpdate, LeasingBalanceUpdate}
import com.wavesplatform.events.api.grpc.protobuf.{GetBlockUpdatesRangeRequest, SubscribeRequest}
import com.wavesplatform.events.protobuf.BlockchainUpdated.Rollback.RollbackType
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.events.protobuf.serde.*
import com.wavesplatform.events.protobuf.{TransactionMetadata, BlockchainUpdated as PBBlockchainUpdated}
import com.wavesplatform.features.BlockchainFeatures.BlockReward
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.protobuf.*
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.protobuf.transaction.DataTransactionData.DataEntry
import com.wavesplatform.protobuf.transaction.InvokeScriptResult
import com.wavesplatform.protobuf.transaction.InvokeScriptResult.{Call, Invocation, Payment}
import com.wavesplatform.settings.{Constants, WavesSettings}
import com.wavesplatform.state.{AssetDescription, EmptyDataEntry, Height, LeaseBalance, StringDataEntry}
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.assets.exchange.OrderType
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.utils.Signed
import com.wavesplatform.transaction.{Asset, GenesisTransaction, PaymentTransaction, TxHelpers}
import io.grpc.StatusException
import monix.execution.Scheduler.Implicits.global
import org.scalactic.source.Position
import org.scalatest.Assertion
import org.scalatest.concurrent.ScalaFutures

import java.util.concurrent.locks.ReentrantLock
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}
import scala.util.Random

class BlockchainUpdatesSpec extends FreeSpec with WithBUDomain with ScalaFutures {
  val currentSettings: WavesSettings = RideV5

  val transfer = TxHelpers.transfer()
  val lease    = TxHelpers.lease(fee = TestValues.fee)
  val issue    = TxHelpers.issue(amount = 1000)
  val reissue  = TxHelpers.reissue(issue.asset)
  val data     = TxHelpers.dataSingle()

  val description = AssetDescription(
    issue.assetId,
    issue.sender,
    issue.name,
    issue.description,
    issue.decimals.value,
    issue.reissuable,
    issue.quantity.value + reissue.quantity.value,
    Height @@ 2,
    None,
    0L,
    nft = false
  )

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(10 seconds, 500 millis)

  "gRPC API" - {
    "return valid errors" in withDomainAndRepo(currentSettings) { (_, repo) =>
      val obs = repo.createFakeObserver(SubscribeRequest(999, 999))
      intercept[Throwable](obs.fetchUntil(_ => false)) should matchPattern {
        case se: StatusException if se.getMessage.contains("Requested start height exceeds current blockchain height") =>
      }
    }
  }

  "BlockchainUpdates" - {
    "should return order ids in exchange metadata" in withDomainAndRepo(DomainPresets.RideV4) { case (d, repo) =>
      val issue = TxHelpers.issue()
      d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress))
      d.appendBlock(issue)

      val subscription = repo.createFakeObserver(SubscribeRequest.of(1, 0))
      val exchange     = TxHelpers.exchangeFromOrders(TxHelpers.orderV3(OrderType.BUY, issue.asset), TxHelpers.orderV3(OrderType.SELL, issue.asset))
      d.appendBlock(exchange)

      subscription.lastAppendEvent(d.blockchain).transactionMetadata should matchPattern {
        case Seq(TransactionMetadata(_, TransactionMetadata.Metadata.Exchange(TransactionMetadata.ExchangeMetadata(ids, _, _, _)), _))
            if ids.map(_.toByteStr) == Seq(exchange.order1.id(), exchange.order2.id()) =>
      }
    }

    "should process nested invoke with args" in withDomainAndRepo(currentSettings) { case (d, repo) =>
      val script = TxHelpers.script("""
                                      |{-# STDLIB_VERSION 5 #-}
                                      |{-# CONTENT_TYPE DAPP #-}
                                      |
                                      |@Callable(inv)
                                      |func foo() = {
                                      |  strict ii = invoke(this, "bar", [1], [])
                                      |  [IntegerEntry("test1", 1)]
                                      |}
                                      |
                                      |@Callable(inv)
                                      |func bar(i: Int) = [IntegerEntry("test", 2)]
                                      |""".stripMargin)

      d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress))
      d.appendBlock(TxHelpers.setScript(TxHelpers.defaultSigner, script))
      d.appendBlock(TxHelpers.invoke(TxHelpers.defaultAddress, Some("foo")))

      val subscription = repo.createFakeObserver(SubscribeRequest.of(1, 0))
      val events       = subscription.fetchAllEvents(d.blockchain).map(_.getUpdate.vanillaAppend)
      val invocations  = events.last.transactionMetadata.head.getInvokeScript.getResult.invokes

      invocations shouldBe List(
        Invocation(
          ByteString.copyFrom(TxHelpers.defaultAddress.bytes),
          Some(Call("bar", args = Seq(Call.Argument(Call.Argument.Value.IntegerValue(1))))),
          stateChanges = Some(InvokeScriptResult(data = Seq(DataEntry("test", DataEntry.Value.IntValue(2)))))
        )
      )
    }

    "should not freeze on micro rollback" in withDomainAndRepo(currentSettings) { case (d, repo) =>
      val keyBlockId = d.appendKeyBlock().id()
      d.appendMicroBlock(TxHelpers.transfer())
      d.appendMicroBlock(TxHelpers.transfer())

      val subscription = repo.createFakeObserver(SubscribeRequest.of(1, 0))
      d.appendKeyBlock(Some(keyBlockId))

      subscription.fetchAllEvents(d.blockchain).map(_.getUpdate) should (
        matchPattern {
          case Seq(
                E.Block(1, _),
                E.Micro(1, _),
                E.Micro(1, _),
                E.MicroRollback(1, `keyBlockId`),
                E.Block(2, _)
              ) =>
        } or matchPattern {
          case Seq(
                E.Block(1, _),
                E.Block(2, _)
              ) =>
        }
      )
    }

    "should not freeze on block rollback" in withDomainAndRepo(currentSettings) { case (d, repo) =>
      val block1Id = d.appendKeyBlock().id()
      d.appendKeyBlock()

      val subscription = repo.createFakeObserver(SubscribeRequest.of(1, 0))
      d.rollbackTo(block1Id)
      d.appendKeyBlock()

      subscription.fetchAllEvents(d.blockchain).map(_.getUpdate) should matchPattern {
        case Seq(
              E.Block(1, _),
              E.Block(2, _),
              E.Rollback(1, `block1Id`),
              E.Block(2, _)
            ) =>
      }
    }

    "should not duplicate blocks" in withDomainAndRepo(currentSettings) { case (d, repo) =>
      for (_ <- 1 to 99) d.appendBlock()
      d.appendKeyBlock()
      d.appendMicroBlock(TxHelpers.transfer())
      d.appendKeyBlock()

      val events = {
        val sub = repo.createFakeObserver(SubscribeRequest.of(1, 0))
        sub.fetchAllEvents(d.blockchain).map(_.getUpdate)
      }

      val lastEvents = events.dropWhile(_.height < 100)
      lastEvents should matchPattern {
        case Seq(
              E.Block(100, _),
              E.Block(101, _)
            ) =>
      }
    }

    "should not freeze on block rollback without key-block" in withDomainAndRepo(currentSettings) { case (d, repo) =>
      val block1Id = d.appendBlock().id()
      val block2Id = d.appendBlock().id()
      d.appendBlock()
      d.rollbackTo(block2Id)

      val subscription = repo.createFakeObserver(SubscribeRequest.of(1, 0))
      d.rollbackTo(block1Id)
      d.appendBlock()

      subscription.fetchAllEvents(d.blockchain).map(_.getUpdate) should matchPattern {
        case Seq(
              E.Block(1, _),
              E.Block(2, _),
              E.Rollback(1, `block1Id`),
              E.Block(2, _)
            ) =>
      }
    }

    "should survive invalid rollback" in withDomainAndRepo(RideV6.copy(dbSettings = dbSettings.copy(maxRollbackDepth = 0))) { (d, repo) =>
      for (_ <- 1 to 10) d.appendBlock()
      intercept[RuntimeException](d.rollbackTo(1)) // Should fail
      d.appendBlock()
      repo.getBlockUpdatesRange(GetBlockUpdatesRangeRequest.of(1, 10)).futureValue.updates.map(_.height) shouldBe (1 to 10)
    }

    "should survive invalid micro rollback" in withDomainAndRepo(currentSettings) { case (d, repo) =>
      d.appendKeyBlock()
      val sub   = repo.createFakeObserver(SubscribeRequest(1))
      val mb1Id = d.appendMicroBlock(TxHelpers.transfer())
      val mb2Id = d.appendMicroBlock(TxHelpers.transfer())
      d.appendMicroBlock(TxHelpers.transfer())

      d.blockchain.removeAfter(mb1Id) // Should not do anything
      d.appendKeyBlock(ref = Some(mb2Id))

      sub.fetchAllEvents(d.blockchain).map(_.getUpdate) should (
        matchPattern {
          case Seq(
                E.Block(1, _),
                E.Micro(1, _),
                E.Micro(1, _),
                E.Micro(1, _),
                E.MicroRollback(1, `mb2Id`),
                E.Block(2, _)
              ) =>
        } or matchPattern {
          case Seq(
                E.Block(1, _),
                E.Micro(1, _),
                E.Micro(1, _),
                E.Block(2, _)
              ) =>
        }
      )
    }

    "should survive rollback to key block" in withDomainAndRepo(currentSettings) { (d, repo) =>
      d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress))
      val subscription = repo.createFakeObserver(SubscribeRequest.of(1, 0))
      val keyBlockId   = d.appendKeyBlock().id()
      d.appendMicroBlock(TxHelpers.transfer())
      d.appendKeyBlock(ref = Some(keyBlockId)) // Remove micro

      subscription.fetchAllEvents(d.blockchain).map(_.getUpdate) should (
        matchPattern {
          case Seq(
                E.Block(1, _),
                E.Block(2, _),
                E.Micro(2, _),
                E.MicroRollback(2, `keyBlockId`),
                E.Block(3, _)
              ) =>
        } or
          matchPattern {
            case Seq(
                  E.Block(1, _),
                  E.Block(2, _),
                  E.Block(3, _)
                ) =>
          }
      )
    }

    "should include correct waves amount" - {
      val totalWaves = 100_000_000_0000_0000L
      val reward     = 6_0000_0000

      "on preactivated block reward" in {
        val settings = currentSettings.setFeaturesHeight((BlockReward, 0))

        withDomainAndRepo(settings) { case (d, repo) =>
          d.appendBlock()
          d.blockchain.wavesAmount(1) shouldBe totalWaves + reward
          repo.getBlockUpdate(1).getUpdate.vanillaAppend.updatedWavesAmount shouldBe totalWaves + reward

          d.appendBlock()
          d.blockchain.wavesAmount(2) shouldBe totalWaves + reward * 2
          repo.getBlockUpdate(2).getUpdate.vanillaAppend.updatedWavesAmount shouldBe totalWaves + reward * 2
        }
      }

      "on activation of block reward" in {
        val settings = currentSettings.setFeaturesHeight((BlockReward, 3))

        withNEmptyBlocksSubscription(settings = settings, count = 3) { result =>
          val balances = result.collect { case b if b.update.isAppend => b.getAppend.getBlock.updatedWavesAmount }
          balances shouldBe Seq(totalWaves, totalWaves, totalWaves + reward, totalWaves + reward * 2)
        }

        withDomainAndRepo(settings) { case (d, repo) =>
          d.appendBlock()
          d.blockchain.wavesAmount(1) shouldBe totalWaves
          repo.getBlockUpdate(1).getUpdate.vanillaAppend.updatedWavesAmount shouldBe totalWaves

          d.appendBlock()
          d.blockchain.wavesAmount(2) shouldBe totalWaves
          repo.getBlockUpdate(2).getUpdate.vanillaAppend.updatedWavesAmount shouldBe totalWaves

          d.appendBlock()
          d.blockchain.wavesAmount(3) shouldBe totalWaves + reward
          repo.getBlockUpdate(3).getUpdate.vanillaAppend.updatedWavesAmount shouldBe totalWaves + reward

          d.appendBlock()
          d.blockchain.wavesAmount(4) shouldBe totalWaves + reward * 2
          repo.getBlockUpdate(4).getUpdate.vanillaAppend.updatedWavesAmount shouldBe totalWaves + reward * 2
        }
      }

      "on rollbacks" in {
        withDomainAndRepo(currentSettings) { case (d, repo) =>
          d.appendBlock()

          // block and micro append
          val block = d.appendBlock()
          block.sender shouldBe defaultSigner.publicKey

          d.appendMicroBlock(TxHelpers.transfer(defaultSigner))
          d.blockchain.wavesAmount(2) shouldBe totalWaves + reward * 2
          repo.getBlockUpdate(2).getUpdate.vanillaAppend.updatedWavesAmount shouldBe totalWaves + reward * 2

          // micro rollback
          d.appendKeyBlock(Some(block.id()))
          d.blockchain.wavesAmount(3) shouldBe totalWaves + reward * 3
          repo.getBlockUpdate(3).getUpdate.vanillaAppend.updatedWavesAmount shouldBe totalWaves + reward * 3

          // block rollback
          d.rollbackTo(2)
          d.blockchain.wavesAmount(2) shouldBe totalWaves + reward * 2
          repo.getBlockUpdate(2).getUpdate.vanillaAppend.updatedWavesAmount shouldBe totalWaves + reward * 2
        }
      }
    }

    "should include correct heights" in withNEmptyBlocksSubscription(settings = currentSettings) { result =>
      val heights = result.map(_.height)
      heights shouldBe Seq(1, 2, 3)
    }

    "should handle toHeight=0" in withNEmptyBlocksSubscription(request = SubscribeRequest.of(1, 0), settings = currentSettings) { result =>
      result should have size 3
    }

    "should handle stream from height 1" in {
      withNEmptyBlocksSubscription(99, SubscribeRequest(1, 60), currentSettings) { updates =>
        updates.map(_.height) shouldBe (1 to 60)
      }

      withNEmptyBlocksSubscription(99, SubscribeRequest(1, 70), currentSettings) { updates =>
        updates.map(_.height) shouldBe (1 to 70)
      }

      withNEmptyBlocksSubscription(99, SubscribeRequest(1, 110), currentSettings) { updates =>
        updates.map(_.height) shouldBe (1 to 100)
      }
    }

    "should handle stream from arbitrary height" in withDomainAndRepo(currentSettings) { (d, repo) =>
      d.appendBlock(TxHelpers.genesis(TxHelpers.defaultSigner.toAddress, Constants.TotalWaves * Constants.UnitsInWave))

      (2 to 10).foreach(_ => d.appendBlock())
      val subscription = repo.createFakeObserver(SubscribeRequest.of(8, 15))
      (1 to 10).foreach(_ => d.appendBlock())

      val result = subscription.fetchAllEvents(d.blockchain, 15)
      result.map(_.getUpdate.height) shouldBe (8 to 15)
    }

    "should handle genesis and payment" in withGenerateSubscription(settings = currentSettings.configure(_.copy(blockVersion3AfterHeight = 3))) { d =>
      val tx =
        PaymentTransaction.create(TxHelpers.defaultSigner, TxHelpers.secondAddress, 100, 100000, TxHelpers.timestamp).explicitGet()
      d.appendBlock(tx)
    } { results =>
      val reward        = 600000000
      val genesisAmount = Constants.TotalWaves * Constants.UnitsInWave + reward
      val genesis       = results.head.getAppend.transactionStateUpdates.head.balances.head
      genesis.address.toAddress shouldBe TxHelpers.defaultAddress
      genesis.getAmountAfter.amount shouldBe genesisAmount
      genesis.amountBefore shouldBe reward
      genesis.getAmountAfter.assetId shouldBe empty

      val payment = results.last.getAppend.transactionStateUpdates.last.balances.find { bu =>
        bu.address.toAddress == TxHelpers.secondAddress
      }.get

      payment.getAmountAfter.amount shouldBe 100
      payment.amountBefore shouldBe 0
      payment.getAmountAfter.assetId shouldBe empty
    }

    "should fail stream with invalid range" in {
      intercept[StatusException](withNEmptyBlocksSubscription(99, SubscribeRequest(0, 60), currentSettings)(_ => ()))
      intercept[StatusException](withNEmptyBlocksSubscription(99, SubscribeRequest(-1, 60), currentSettings)(_ => ()))
      intercept[StatusException](withNEmptyBlocksSubscription(99, SubscribeRequest(300, 60), currentSettings)(_ => ()))
    }

    "should return issued assets" in {
      val issue = TxHelpers.issue()
      val description = AssetDescription(
        issue.assetId,
        issue.sender,
        issue.name,
        issue.description,
        issue.decimals.value,
        issue.reissuable,
        issue.quantity.value,
        Height @@ 2,
        None,
        0L,
        nft = false
      )

      withGenerateSubscription(settings = currentSettings) { d =>
        d.appendBlock(issue)
      } { events =>
        val event  = events.last
        val issued = event.getAppend.transactionStateUpdates.head.assets
        issued shouldBe Seq(AssetStateUpdate.toPB(AssetStateUpdate(issue.assetId, None, Some(description))))
        event.referencedAssets shouldBe Seq(AssetInfo.toPB(AssetInfo(issue.assetId, description.decimals, description.name.toStringUtf8)))
      }
    }

    "should return correct content of block rollback" in {
      var sendUpdate: () => Unit = null
      withManualHandle(currentSettings, sendUpdate = _) { case (d, repo) =>
        d.appendBlock(TxHelpers.genesis(TxHelpers.defaultSigner.toAddress, Constants.TotalWaves * Constants.UnitsInWave))
        d.appendKeyBlock()

        val subscription = repo.createFakeObserver(SubscribeRequest.of(1, 0))
        sendUpdate()
        sendUpdate()

        d.appendMicroBlock(transfer, lease, issue, reissue, data)
        sendUpdate()

        d.appendKeyBlock()
        sendUpdate()

        d.rollbackTo(1)
        sendUpdate()
        sendUpdate()

        val rollbackEvent = subscription.fetchAllEvents(d.blockchain).findLast(_.getUpdate.update.isRollback)
        val rollback      = vanillaRollback(rollbackEvent.get.getUpdate).rollbackResult

        rollback.removedBlocks should have length 1
        rollback.stateUpdate.balances shouldBe Seq(
          BalanceUpdate(TxHelpers.defaultAddress, Waves, 10000001036400000L, after = 10000000600000000L),
          BalanceUpdate(TxHelpers.defaultAddress, issue.asset, 2000, after = 0),
          BalanceUpdate(TxHelpers.secondAddress, Waves, 100000000, after = 0)
        )
        assertCommon(rollback)
      }
    }

    "should return correct content of microblock rollback" in {
      var sendUpdate: () => Unit = null
      withManualHandle(currentSettings, sendUpdate = _) { case (d, repo) =>
        d.appendBlock(TxHelpers.genesis(TxHelpers.defaultSigner.toAddress, Constants.TotalWaves * Constants.UnitsInWave))
        d.appendKeyBlock()

        val subscription = repo.createFakeObserver(SubscribeRequest.of(1, 0))
        sendUpdate()
        sendUpdate()

        val firstMicroId = d.appendMicroBlock(TxHelpers.transfer())
        sendUpdate()

        d.appendMicroBlock(transfer, lease, issue, reissue, data)
        sendUpdate()

        d.appendKeyBlock(Some(firstMicroId))
        sendUpdate()
        sendUpdate()

        val rollbackEvent = subscription.fetchAllEvents(d.blockchain).findLast(_.getUpdate.update.isRollback)
        val rollback      = vanillaMicroRollback(rollbackEvent.get.getUpdate).rollbackResult

        rollback.removedBlocks shouldBe empty
        rollback.stateUpdate.balances shouldBe Seq(
          BalanceUpdate(TxHelpers.defaultAddress, Waves, 10000000935800000L, after = 10000001099400000L),
          BalanceUpdate(TxHelpers.defaultAddress, issue.asset, 2000, after = 0),
          BalanceUpdate(TxHelpers.secondAddress, Waves, 200000000, after = 100000000)
        )
        assertCommon(rollback)
      }
    }

    "should skip rollback in real time updates" in withDomainAndRepo(currentSettings) { (d, repo) =>
      d.appendKeyBlock()
      d.appendKeyBlock()
      d.rollbackTo(1)
      d.appendKeyBlock()
      d.appendKeyBlock()

      val subscription = repo.createFakeObserver(SubscribeRequest(1))
      subscription.fetchAllEvents(d.blockchain).map(_.getUpdate.height) shouldBe Seq(1, 2, 3)
    }

    "should clear event queue on microblock rollback to block if it was not sent" in {
      var sendUpdate: () => Unit = null
      withManualHandle(currentSettings, sendUpdate = _) { case (d, repo) =>
        val keyBlockId   = d.appendKeyBlock().id()
        val subscription = repo.createFakeObserver(SubscribeRequest.of(1, 0))

        d.appendMicroBlock(TxHelpers.transfer())
        d.appendMicroBlock(TxHelpers.transfer())
        d.appendKeyBlock(Some(keyBlockId))

        sendUpdate()
        sendUpdate()

        subscription.fetchAllEvents(d.blockchain).map(_.getUpdate) should matchPattern {
          case Seq(
                E.Block(1, _),
                E.Block(2, _)
              ) =>
        }
      }
    }

    "should clear event queue on rollback to microblock if it was not sent" in {
      var sendUpdate: () => Unit = null
      withManualHandle(currentSettings, sendUpdate = _) { case (d, repo) =>
        d.appendKeyBlock().id()
        val subscription = repo.createFakeObserver(SubscribeRequest.of(1, 0))

        val microBlockId = d.appendMicroBlock(TxHelpers.transfer())
        d.appendMicroBlock(TxHelpers.transfer())
        d.appendKeyBlock(Some(microBlockId))

        (1 to 3).foreach(_ => sendUpdate())

        subscription.fetchAllEvents(d.blockchain).map(_.getUpdate) should matchPattern {
          case Seq(
                E.Block(1, _),
                E.Micro(1, `microBlockId`),
                E.Block(2, _)
              ) =>
        }
      }
    }

    "should clear event queue on microblock rollback to block if it was sent but microblock after wasn't" in {
      var sendUpdate: () => Unit = null
      withManualHandle(currentSettings, sendUpdate = _) { case (d, repo) =>
        val keyBlockId   = d.appendKeyBlock().id()
        val subscription = repo.createFakeObserver(SubscribeRequest.of(1, 0))
        sendUpdate()

        d.appendMicroBlock(TxHelpers.transfer())
        d.appendMicroBlock(TxHelpers.transfer())
        d.appendKeyBlock(Some(keyBlockId))
        sendUpdate()

        subscription.fetchAllEvents(d.blockchain).map(_.getUpdate) should matchPattern {
          case Seq(
                E.Block(1, _),
                E.Block(2, _)
              ) =>
        }
      }
    }

    "should send event on microblock rollback if first microblock after was sent" in {
      var sendUpdate: () => Unit = null
      withManualHandle(currentSettings, sendUpdate = _) { case (d, repo) =>
        val keyBlockId   = d.appendKeyBlock().id()
        val subscription = repo.createFakeObserver(SubscribeRequest.of(1, 0))
        sendUpdate()

        d.appendMicroBlock(TxHelpers.transfer())
        sendUpdate()

        d.appendMicroBlock(TxHelpers.transfer())
        d.appendKeyBlock(Some(keyBlockId))

        (1 to 3).foreach(_ => sendUpdate())

        subscription.fetchAllEvents(d.blockchain).map(_.getUpdate) should matchPattern {
          case Seq(
                E.Block(1, _),
                E.Micro(1, _),
                E.Micro(1, _),
                E.MicroRollback(1, `keyBlockId`),
                E.Block(2, _)
              ) =>
        }
      }
    }

    "should get valid range" in withDomainAndRepo(currentSettings) { (d, repo) =>
      for (_ <- 1 to 10) d.appendBlock()
      val blocks = repo.getBlockUpdatesRange(GetBlockUpdatesRangeRequest(3, 5)).futureValue.updates
      blocks.map(_.height) shouldBe Seq(3, 4, 5)
    }

    "should return correct ids for assets and leases from invoke" in withDomainAndRepo(currentSettings) { (d, repo) =>
      val issuer        = KeyPair(Longs.toByteArray(Random.nextLong()))
      val invoker       = KeyPair(Longs.toByteArray(Random.nextLong()))
      val issuerAddress = issuer.toAddress
      val dAppScript = TestCompiler(V5).compileContract(
        s"""
           |@Callable(i)
           |func issue() = {
           |  let issue = Issue("name", "description", 1000, 4, true, unit, 0)
           |  let lease = Lease(i.caller, 500000000)
           |  [
           |    issue,
           |    BinaryEntry("assetId", calculateAssetId(issue)),
           |    lease,
           |    BinaryEntry("leaseId", calculateLeaseId(lease))
           |  ]
           |}
         """.stripMargin
      )
      val invoke = Signed.invokeScript(
        2.toByte,
        invoker,
        issuer.toAddress,
        Some(FUNCTION_CALL(FunctionHeader.User("issue"), Nil)),
        Seq.empty,
        2.waves,
        Asset.Waves,
        ntpTime.getTimestamp()
      )
      d.appendBlock(
        GenesisTransaction.create(issuerAddress, 1000.waves, ntpTime.getTimestamp()).explicitGet(),
        GenesisTransaction.create(invoker.toAddress, 1000.waves, ntpTime.getTimestamp()).explicitGet(),
        SetScriptTransaction.selfSigned(2.toByte, issuer, Some(dAppScript), 0.01.waves, ntpTime.getTimestamp()).explicitGet(),
        invoke
      )

      val assetId = d.blockchain.binaryData(issuer.toAddress, "assetId").get
      val leaseId = d.blockchain.binaryData(issuerAddress, "leaseId").get

      def check()(implicit pos: Position): Unit = {
        val genesisUpdate = repo.getBlockUpdate(1).getUpdate.vanillaAppend
        genesisUpdate.referencedAssets.head.id shouldEqual assetId
        val leaseUpdate = genesisUpdate.transactionStateUpdates(3).leases.head
        leaseUpdate.originTransactionId shouldEqual invoke.id()
        leaseUpdate.leaseId shouldEqual leaseId
      }

      check()

      d.appendBlock()

      check()
    }

    "should correctly concatenate stream from DB and new blocks stream" in {
      subscribeAndCheckResult(5, _ => (), 1 to 5)
      subscribeAndCheckResult(5, d => d.appendMicroBlock(TxHelpers.transfer()), (1 to 5) :+ 5)
      subscribeAndCheckResult(5, d => d.appendKeyBlock(), 1 to 5, isStreamClosed = true)
      subscribeAndCheckResult(
        5,
        d => {
          d.appendMicroBlock(TxHelpers.transfer())
          d.appendKeyBlock()
        },
        (1 to 5) :+ 5,
        isStreamClosed = true
      )
      subscribeAndCheckResult(0, _ => (), 1 to 5)
      subscribeAndCheckResult(0, d => d.appendMicroBlock(TxHelpers.transfer()), (1 to 5) :+ 5)
      subscribeAndCheckResult(0, d => d.appendKeyBlock(), (1 to 5) :+ 6)
      subscribeAndCheckResult(
        0,
        d => {
          d.appendMicroBlock(TxHelpers.transfer())
          d.appendKeyBlock()
        },
        (1 to 5) ++ Seq(5, 6)
      )
      subscribeAndCheckResult(0, d => { (1 to 249).foreach(_ => d.appendMicroBlock(TxHelpers.transfer(amount = 1))) }, (1 to 4) ++ Seq.fill(250)(5))
      subscribeAndCheckResult(0, d => { (1 to 250).foreach(_ => d.appendMicroBlock(TxHelpers.transfer(amount = 1))) }, 1 to 4, isStreamClosed = true)
    }

    "should return address for invoke transfer recipient" in {
      val dApp1 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict r = invoke(Address(base58'$secondAddress'), "default", [], [])
           |   [ScriptTransfer(Address(base58'$secondAddress'), 1, unit)]
           | }
        """.stripMargin
      )
      val dApp2 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   [ScriptTransfer(Address(base58'${signer(2).toAddress}'), 1, unit)]
           | }
        """.stripMargin
      )
      withGenerateSubscription(settings = currentSettings) { d =>
        d.appendBlock(transfer)
        d.appendBlock(setScript(defaultSigner, dApp1), setScript(secondSigner, dApp2))
        d.appendAndAssertSucceed(invoke(defaultAddress))
      } { events =>
        val invokeResult              = events.last.getAppend.transactionsMetadata.head.getInvokeScript.getResult
        def payment(address: Address) = Seq(Payment(ByteString.copyFrom(address.bytes), Some(Amount.of(ByteString.EMPTY, 1))))
        invokeResult.transfers shouldBe payment(secondAddress)
        invokeResult.invokes.head.stateChanges.get.transfers shouldBe payment(signer(2).toAddress)
      }
    }
  }

  private def assertCommon(rollback: RollbackResult): Assertion = {
    rollback.stateUpdate.leasingForAddress shouldBe Seq(
      LeasingBalanceUpdate(TxHelpers.secondAddress, LeaseBalance(1000000000, 0), LeaseBalance(0, 0)),
      LeasingBalanceUpdate(TxHelpers.defaultAddress, LeaseBalance(0, 1000000000), LeaseBalance(0, 0))
    )
    rollback.stateUpdate.leases shouldBe Seq(
      LeaseUpdate(lease.id(), LeaseStatus.Inactive, lease.amount.value, lease.sender, lease.recipient.asInstanceOf[Address], lease.id())
    )
    rollback.stateUpdate.dataEntries shouldBe Seq(
      DataEntryUpdate(TxHelpers.defaultAddress, StringDataEntry("test", "test"), EmptyDataEntry("test"))
    )
    rollback.stateUpdate.assets shouldBe Seq(
      AssetStateUpdate(issue.assetId, Some(description), None)
    )
    rollback.removedTransactionIds shouldBe Seq(data, reissue, issue, lease, transfer).map(_.id())
  }

  private def subscribeAndCheckResult(
      toHeight: Int,
      appendExtraBlocks: Domain => Unit,
      expectedResult: Seq[Int],
      isStreamClosed: Boolean = false
  ): Unit = {
    val startRead = new ReentrantLock()
    withDomainAndRepo(currentSettings)(
      { (d, repo) =>
        (1 to 5).foreach(_ => d.appendBlock())

        startRead.lock()

        val subscription = Future(repo.createSubscriptionObserver(SubscribeRequest.of(1, toHeight)))

        appendExtraBlocks(d)

        startRead.unlock()

        val timeout = 30.seconds
        Await
          .result(
            subscription.map(s => s.fetchUntil(_.map(_.getUpdate.height) == expectedResult && s.completed == isStreamClosed, timeout)),
            timeout
          )
      },
      db => InterferableDB(db, startRead)
    )
  }

  def vanillaRollback(self: PBBlockchainUpdated): RollbackCompleted = self.update match {
    case Update.Rollback(rollback) if rollback.`type` == RollbackType.BLOCK =>
      RollbackCompleted(
        self.id.toByteStr,
        self.height,
        RollbackResult(
          rollback.removedBlocks.map(PBBlocks.vanilla(_).get),
          rollback.removedTransactionIds.map(_.toByteStr),
          rollback.getRollbackStateUpdate.vanilla.get
        ),
        referencedAssets = self.referencedAssets.map(AssetInfo.fromPB)
      )
    case _ => throw new IllegalArgumentException("Not a block rollback")
  }

  def vanillaMicroRollback(self: PBBlockchainUpdated): MicroBlockRollbackCompleted = self.update match {
    case Update.Rollback(rollback) if rollback.`type` == RollbackType.MICROBLOCK =>
      MicroBlockRollbackCompleted(
        id = self.id.toByteStr,
        height = self.height,
        RollbackResult.micro(rollback.removedTransactionIds.map(_.toByteStr), rollback.getRollbackStateUpdate.vanilla.get),
        referencedAssets = self.referencedAssets.map(AssetInfo.fromPB)
      )
    case _ => throw new IllegalArgumentException("Not a microblock rollback")
  }
}
