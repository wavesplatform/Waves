package com.wavesplatform.events

import com.google.common.primitives.Longs
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.database.openDB
import com.wavesplatform.db.WithDomain
import com.wavesplatform.events.StateUpdate.LeaseUpdate.LeaseStatus
import com.wavesplatform.events.StateUpdate.{AssetInfo, AssetStateUpdate, BalanceUpdate, DataEntryUpdate, LeaseUpdate, LeasingBalanceUpdate}
import com.wavesplatform.events.api.grpc.protobuf.{GetBlockUpdatesRangeRequest, SubscribeEvent, SubscribeRequest}
import com.wavesplatform.events.protobuf.BlockchainUpdated.Rollback.RollbackType
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.events.protobuf.serde._
import com.wavesplatform.events.protobuf.{TransactionMetadata, BlockchainUpdated => PBBlockchainUpdated}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.protobuf._
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.protobuf.transaction.DataTransactionData.DataEntry
import com.wavesplatform.protobuf.transaction.InvokeScriptResult
import com.wavesplatform.protobuf.transaction.InvokeScriptResult.{Call, Invocation}
import com.wavesplatform.settings.{Constants, FunctionalitySettings, TestFunctionalitySettings, WavesSettings}
import com.wavesplatform.state.{AssetDescription, Blockchain, EmptyDataEntry, Height, LeaseBalance, StringDataEntry}
import com.wavesplatform.test._
import com.wavesplatform.test.DomainPresets._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.OrderType
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{Asset, GenesisTransaction, PaymentTransaction, TxHelpers}
import io.grpc.StatusException
import io.grpc.stub.{CallStreamObserver, StreamObserver}
import monix.eval.Task
import monix.execution.CancelableFuture
import monix.execution.Scheduler.Implicits.global
import org.iq80.leveldb
import org.iq80.leveldb._
import org.scalactic.source.Position
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.concurrent.ScalaFutures

import java.nio.file.Files
import java.util.Map
import java.util.concurrent.locks.ReentrantLock
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.Random

class BlockchainUpdatesSpec extends FreeSpec with WithDomain with ScalaFutures with PathMockFactory {
  var currentSettings: WavesSettings = domainSettingsWithFS(
    TestFunctionalitySettings.withFeatures(
      BlockchainFeatures.BlockReward,
      BlockchainFeatures.NG,
      BlockchainFeatures.SmartAccounts,
      BlockchainFeatures.DataTransaction,
      BlockchainFeatures.FeeSponsorship,
      BlockchainFeatures.BlockV5,
      BlockchainFeatures.SynchronousCalls
    )
  )

  def currentFS: FunctionalitySettings = currentSettings.blockchainSettings.functionalitySettings

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(10 seconds, 500 millis)

  "gRPC API" - {
    "return valid errors" in withRepo() { repo =>
      val (obs, result) = createFakeObserver[SubscribeEvent]()
      repo.subscribe(SubscribeRequest(999, 999), obs)
      result.failed.futureValue should matchPattern {
        case se: StatusException if se.getMessage.contains("Requested start height exceeds current blockchain height") =>
      }
    }
  }

  "BlockchainUpdates" - {
    "should return order ids in exchange metadata" in withSettings(DomainPresets.RideV4)(withDomainAndRepo {
      case (d, repo) =>
        val issue = TxHelpers.issue()
        d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress))
        d.appendBlock(issue)

        val subscription = repo.createSubscriptionObserver(SubscribeRequest.of(1, 0))
        val exchange     = TxHelpers.exchangeFromOrders(TxHelpers.orderV3(OrderType.BUY, issue.asset), TxHelpers.orderV3(OrderType.SELL, issue.asset))
        d.appendBlock(exchange)

        subscription.lastAppendEvent(d.blockchain).transactionMetadata should matchPattern {
          case Seq(TransactionMetadata(TransactionMetadata.Metadata.Exchange(TransactionMetadata.ExchangeMetadata(ids, _)), _))
              if ids.map(_.toByteStr) == Seq(exchange.order1.id(), exchange.order2.id()) =>
        }
    })

    "should process nested invoke with args" in withDomainAndRepo {
      case (d, repo) =>
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
            |
        """.stripMargin)

        d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress))
        d.appendBlock(TxHelpers.setScript(TxHelpers.defaultSigner, script))
        d.appendBlock(TxHelpers.invoke(TxHelpers.defaultAddress, Some("foo")))
        val subscription = repo.createSubscription(SubscribeRequest.of(1, 0))
        Thread.sleep(1000)
        subscription.cancel()
        val events      = subscription.futureValue.map(_.getUpdate.vanillaAppend)
        val invocations = events.last.transactionMetadata.head.getInvokeScript.getResult.invokes
        invocations shouldBe List(
          Invocation(
            ByteString.copyFrom(TxHelpers.defaultAddress.bytes),
            Some(Call("bar", args = Seq(Call.Argument(Call.Argument.Value.IntegerValue(1))))),
            stateChanges = Some(InvokeScriptResult(data = Seq(DataEntry("test", DataEntry.Value.IntValue(2)))))
          )
        )
    }

    "should not freeze on micro rollback" in withDomainAndRepo {
      case (d, repo) =>
        val keyBlockId = d.appendKeyBlock().id()
        d.appendMicroBlock(TxHelpers.transfer())
        d.appendMicroBlock(TxHelpers.transfer())

        val subscription = repo.createSubscription(SubscribeRequest.of(1, 0))
        Thread.sleep(1000)
        d.appendKeyBlock(Some(keyBlockId))

        Thread.sleep(1000)
        subscription.cancel()
        subscription.futureValue.map(_.getUpdate) should matchPattern {
          case Seq(
              E.Block(1, _),
              E.Micro(1, _),
              E.Micro(1, _),
              E.MicroRollback(1, `keyBlockId`),
              E.Block(2, _)
              ) =>
        }
    }

    "should not freeze on block rollback" in withDomainAndRepo {
      case (d, repo) =>
        val block1Id = d.appendKeyBlock().id()
        d.appendKeyBlock()

        val subscription = repo.createSubscription(SubscribeRequest.of(1, 0))
        Thread.sleep(1000)
        d.rollbackTo(block1Id)
        d.appendKeyBlock()

        Thread.sleep(1000)
        subscription.cancel()
        subscription.futureValue.map(_.getUpdate) should matchPattern {
          case Seq(
              E.Block(1, _),
              E.Block(2, _),
              E.Rollback(1, `block1Id`),
              E.Block(2, _)
              ) =>
        }
    }

    "should not duplicate blocks" in withDomainAndRepo {
      case (d, repo) =>
        for (_ <- 1 to 99) d.appendBlock()
        d.appendKeyBlock()
        d.appendMicroBlock(TxHelpers.transfer())
        d.appendKeyBlock()

        val events = {
          val sub = repo.createSubscription(SubscribeRequest.of(1, 0))
          Thread.sleep(1000)
          sub.cancel()
          sub.futureValue.map(_.getUpdate)
        }

        val lastEvents = events.dropWhile(_.height < 100)
        lastEvents should matchPattern {
          case Seq(
              E.Block(100, _),
              E.Block(101, _)
              ) =>
        }
    }

    "should not freeze on block rollback without key-block" in withDomainAndRepo {
      case (d, repo) =>
        val block1Id = d.appendBlock().id()
        val block2Id = d.appendBlock().id()
        d.appendBlock()
        d.rollbackTo(block2Id)

        val subscription = repo.createSubscription(SubscribeRequest.of(1, 0))
        Thread.sleep(1000)
        d.rollbackTo(block1Id)
        d.appendBlock()

        Thread.sleep(1000)
        subscription.cancel()
        subscription.futureValue.map(_.getUpdate) should matchPattern {
          case Seq(
              E.Block(1, _),
              E.Block(2, _),
              E.Rollback(1, `block1Id`),
              E.Block(2, _)
              ) =>
        }
    }

    "should survive invalid rollback" in withDomain(
      SettingsFromDefaultConfig.copy(dbSettings = SettingsFromDefaultConfig.dbSettings.copy(maxRollbackDepth = 0))
    ) { d =>
      withRepo(d.blocksApi) { repo =>
        d.triggers = Seq(repo)
        for (_ <- 1 to 10) d.appendBlock()
        intercept[RuntimeException](d.rollbackTo(1)) // Should fail
        d.appendBlock()
        repo.getBlockUpdatesRange(GetBlockUpdatesRangeRequest.of(1, 10)).futureValue.updates.map(_.height) shouldBe (1 to 10)
      }
    }

    "should survive invalid micro rollback" in withDomainAndRepo {
      case (d, repo) =>
        d.appendKeyBlock()
        val sub   = repo.createSubscription(SubscribeRequest(1))
        val mb1Id = d.appendMicroBlock(TxHelpers.transfer())
        val mb2Id = d.appendMicroBlock(TxHelpers.transfer())
        d.appendMicroBlock(TxHelpers.transfer())

        d.blockchain.removeAfter(mb1Id) // Should not do anything
        d.appendKeyBlock(ref = Some(mb2Id))

        Thread.sleep(1000)
        sub.cancel()
        sub.futureValue.map(_.getUpdate) should matchPattern {
          case Seq(
              E.Block(1, _),
              E.Micro(1, _),
              E.Micro(1, _),
              E.Micro(1, _),
              E.MicroRollback(1, `mb2Id`),
              E.Block(2, _)
              ) =>
        }
    }

    "should survive rollback to key block" in withDomainAndRepo { (d, repo) =>
      d.appendBlock(TxHelpers.genesis(TxHelpers.defaultAddress))
      val subscription = repo.createSubscription(SubscribeRequest.of(1, 0))
      val keyBlockId   = d.appendKeyBlock().id()
      d.appendMicroBlock(TxHelpers.transfer())
      d.appendKeyBlock(ref = Some(keyBlockId)) // Remove micro

      Thread.sleep(1000)
      subscription.cancel()
      subscription.futureValue.map(_.getUpdate) should matchPattern {
        case Seq(
            E.Block(1, _),
            E.Block(2, _),
            E.Micro(2, _),
            E.MicroRollback(2, `keyBlockId`),
            E.Block(3, _)
            ) =>
      }
    }

    "should include correct waves amount" in withNEmptyBlocksSubscription() { result =>
      val balances = result.collect { case b if b.update.isAppend => b.getAppend.getBlock.updatedWavesAmount }
      balances shouldBe Seq(10000000000000000L, 10000000600000000L, 10000001200000000L)
    }

    "should include correct heights" in withNEmptyBlocksSubscription() { result =>
      val heights = result.map(_.height)
      heights shouldBe Seq(1, 2, 3)
    }

    "should handle toHeight=0" in withNEmptyBlocksSubscription(request = SubscribeRequest.of(1, 0)) { result =>
      result should have size 3
    }

    "should handle stream from height 1" in {
      withNEmptyBlocksSubscription(99, SubscribeRequest(1, 60)) { updates =>
        updates.map(_.height) shouldBe (1 to 60)
      }

      withNEmptyBlocksSubscription(99, SubscribeRequest(1, 70)) { updates =>
        updates.map(_.height) shouldBe (1 to 70)
      }

      withNEmptyBlocksSubscription(99, SubscribeRequest(1, 110)) { updates =>
        updates.map(_.height) shouldBe (1 to 100)
      }
    }

    "should handle stream from arbitrary height" in withDomainAndRepo { (d, repo) =>
      d.appendBlock(TxHelpers.genesis(TxHelpers.defaultSigner.toAddress, Constants.TotalWaves * Constants.UnitsInWave))

      (1 until 10).foreach(_ => d.appendBlock())
      val subscription = repo.createSubscription(SubscribeRequest.of(8, 15))
      (1 to 10).foreach(_ => d.appendBlock())

      val result = Await.result(subscription, 20 seconds)
      result.map(_.getUpdate.height) shouldBe (8 to 15)
    }

    "should handle genesis and payment" in withFuncSettings(currentFS.copy(blockVersion3AfterHeight = 3))(withGenerateSubscription() { d =>
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

      val payment = results.last.getAppend.transactionStateUpdates.last.balances.last
      payment.address.toAddress shouldBe TxHelpers.secondAddress
      payment.getAmountAfter.amount shouldBe 100
      payment.amountBefore shouldBe 0
      payment.getAmountAfter.assetId shouldBe empty
    })

    "should fail stream with invalid range" in {
      intercept[StatusException](withNEmptyBlocksSubscription(99, SubscribeRequest(0, 60))(_ => ()))
      intercept[StatusException](withNEmptyBlocksSubscription(99, SubscribeRequest(-1, 60))(_ => ()))
      intercept[StatusException](withNEmptyBlocksSubscription(99, SubscribeRequest(300, 60))(_ => ()))
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

      withGenerateSubscription() { d =>
        d.appendBlock(issue)
      } { events =>
        val event  = events.last
        val issued = event.getAppend.transactionStateUpdates.head.assets
        issued shouldBe Seq(AssetStateUpdate.toPB(AssetStateUpdate(issue.assetId, None, Some(description))))
        event.referencedAssets shouldBe Seq(AssetInfo.toPB(AssetInfo(issue.assetId, description.decimals, description.name.toStringUtf8)))
      }
    }

    "should handle rollback properly" in {
      val transfer = TxHelpers.transfer()
      val lease    = TxHelpers.lease()
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

      withGenerateSubscription() { d =>
        d.appendKeyBlock()
        d.appendMicroBlock(transfer, lease, issue, reissue, data)
        d.appendKeyBlock()
        d.rollbackTo(1)
      } { events =>
        val rollback: RollbackResult = events.collect { case bu if bu.update.isRollback => vanillaRollback(bu).rollbackResult }(1)
        rollback.removedTransactionIds shouldBe Seq(data, reissue, issue, lease, transfer).map(_.id())
        rollback.removedBlocks should have length 1

        rollback.stateUpdate.balances shouldBe Seq(
          BalanceUpdate(TxHelpers.defaultAddress, Waves, 10000001036400000L, after = 10000000600000000L),
          BalanceUpdate(TxHelpers.defaultAddress, issue.asset, 2000, after = 0),
          BalanceUpdate(TxHelpers.secondAddress, Waves, 100000000, after = 0)
        )

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
      }

      withGenerateSubscription() { d =>
        d.appendKeyBlock()
        val firstMicroId = d.appendMicroBlock(TxHelpers.transfer())
        d.appendMicroBlock(transfer, lease, issue, reissue, data)
        d.appendKeyBlock(Some(firstMicroId))
      } { events =>
        val rollback: RollbackResult = events.collectFirst { case r if r.update.isRollback => vanillaMicroRollback(r).rollbackResult }.get
        rollback.removedTransactionIds shouldBe Seq(data, reissue, issue, lease, transfer).map(_.id())
        rollback.removedBlocks shouldBe empty

        rollback.stateUpdate.balances shouldBe Seq(
          BalanceUpdate(TxHelpers.defaultAddress, Waves, 10000000935800000L, after = 10000001099400000L),
          BalanceUpdate(TxHelpers.defaultAddress, issue.asset, 2000, after = 0),
          BalanceUpdate(TxHelpers.secondAddress, Waves, 200000000, after = 100000000)
        )

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
      }
    }

    "should skip rollback in real time updates" in withDomainAndRepo { (d, repo) =>
      d.appendKeyBlock()
      d.appendKeyBlock()
      d.rollbackTo(1)
      d.appendKeyBlock()
      d.appendKeyBlock()

      val subscription = repo.createSubscription(SubscribeRequest(1))
      Thread.sleep(1000)
      subscription.cancel()
      subscription.futureValue.map(_.getUpdate.height) shouldBe Seq(1, 2, 3)
    }

    "should get valid range" in withDomainAndRepo { (d, repo) =>
      for (_ <- 1 to 10) d.appendBlock()
      val blocks = repo.getBlockUpdatesRange(GetBlockUpdatesRangeRequest(3, 5)).futureValue.updates
      blocks.map(_.height) shouldBe Seq(3, 4, 5)
    }

    "should return correct ids for assets and leases from invoke" in withDomainAndRepo { (d, repo) =>
      val issuer        = KeyPair(Longs.toByteArray(Random.nextLong()))
      val invoker       = KeyPair(Longs.toByteArray(Random.nextLong()))
      val issuerAddress = issuer.toAddress
      val (dAppScript, _) = ScriptCompiler
        .compile(
          s"""
           |{-# STDLIB_VERSION 5 #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |{-# CONTENT_TYPE DAPP #-}
           |
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
           |""".stripMargin,
          ScriptEstimatorV3(fixOverflow = true)
        )
        .explicitGet()
      val invoke = InvokeScriptTransaction
        .selfSigned(
          2.toByte,
          invoker,
          issuer.toAddress,
          Some(FUNCTION_CALL(FunctionHeader.User("issue"), Nil)),
          Seq.empty,
          2.waves,
          Asset.Waves,
          ntpTime.getTimestamp()
        )
        .explicitGet()
      d.appendBlock(
        GenesisTransaction.create(issuerAddress, 1000.waves, ntpTime.getTimestamp()).explicitGet(),
        GenesisTransaction.create(invoker.toAddress, 1000.waves, ntpTime.getTimestamp()).explicitGet(),
        SetScriptTransaction.selfSigned(2.toByte, issuer, Some(dAppScript), 0.005.waves, ntpTime.getTimestamp()).explicitGet(),
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

    "should handle modifying last block correctly" in {
      val startRead = new ReentrantLock()
      withDomainAndRepo(
        { (d, repo) =>
          (1 to 5).foreach(_ => d.appendBlock())
          startRead.lock()

          val subscription = Future {
            val s = repo.createSubscription(SubscribeRequest.of(1, 5))
            s.cancel()
            s
          }.flatten

          d.appendMicroBlock(TxHelpers.transfer())
          d.appendKeyBlock()

          startRead.unlock()

          Await
            .result(subscription, 5 seconds)
            .map(_.getUpdate.height) shouldBe (1 to 4)
        },
        Some(InterferableDB(startRead))
      )
    }
  }

  case class InterferableDB(startRead: ReentrantLock) extends DB {
    private val db = openDB(Files.createTempDirectory("bc-updates").toString)

    override def get(key: Array[Byte], options: ReadOptions): Array[Byte] = db.get(key, options)
    override def put(key: Array[Byte], value: Array[Byte]): Unit          = db.put(key, value)
    override def getSnapshot: Snapshot                                    = db.getSnapshot
    override def close(): Unit                                            = db.close()

    override def get(key: Array[Byte]): Array[Byte]                                         = ???
    override def delete(key: Array[Byte]): Unit                                             = ???
    override def write(updates: WriteBatch): Unit                                           = ???
    override def createWriteBatch(): WriteBatch                                             = ???
    override def put(key: Array[Byte], value: Array[Byte], options: WriteOptions): Snapshot = ???
    override def delete(key: Array[Byte], options: WriteOptions): Snapshot                  = ???
    override def write(updates: WriteBatch, options: WriteOptions): Snapshot                = ???
    override def getApproximateSizes(ranges: leveldb.Range*): Array[Long]                   = ???
    override def getProperty(name: String): String                                          = ???
    override def suspendCompactions(): Unit                                                 = ???
    override def resumeCompactions(): Unit                                                  = ???
    override def compactRange(begin: Array[Byte], end: Array[Byte]): Unit                   = ???
    override def iterator(): DBIterator                                                     = ???

    override def iterator(options: ReadOptions): DBIterator = new DBIterator {
      private val iterator = db.iterator()
      startRead.lock()

      override def next(): Map.Entry[Array[Byte], Array[Byte]] = iterator.next()
      override def close(): Unit                               = iterator.close()
      override def seek(key: Array[Byte]): Unit                = iterator.seek(key)
      override def hasNext: Boolean                            = iterator.hasNext

      override def seekToFirst(): Unit                             = ???
      override def peekNext(): Map.Entry[Array[Byte], Array[Byte]] = ???
      override def hasPrev: Boolean                                = ???
      override def prev(): Map.Entry[Array[Byte], Array[Byte]]     = ???
      override def peekPrev(): Map.Entry[Array[Byte], Array[Byte]] = ???
      override def seekToLast(): Unit                              = ???
    }
  }

  def withDomainAndRepo(f: (Domain, Repo) => Unit, dbOpt: Option[DB] = None): Unit = {
    withDomain(currentSettings) { d =>
      withRepo(d.blocksApi, dbOpt) { repo =>
        d.triggers = Seq(repo)
        f(d, repo)
      }
    }
  }

  def withGenerateSubscription(request: SubscribeRequest = SubscribeRequest.of(1, Int.MaxValue))(generateBlocks: Domain => Unit)(
      f: Seq[PBBlockchainUpdated] => Unit
  ): Unit = {
    withDomainAndRepo { (d, repo) =>
      d.appendBlock(TxHelpers.genesis(TxHelpers.defaultSigner.toAddress, Constants.TotalWaves * Constants.UnitsInWave))

      val subscription = repo.createSubscription(request)
      generateBlocks(d)
      Thread.sleep(1000)
      subscription.cancel()

      val result = Await.result(subscription, 20 seconds)
      f(result.map(_.getUpdate))
    }
  }

  def withFuncSettings(settings: FunctionalitySettings)(f: => Unit): Unit = {
    val oldSettings = currentSettings
    currentSettings = oldSettings.copy(blockchainSettings = oldSettings.blockchainSettings.copy(functionalitySettings = settings))
    f
    currentSettings = oldSettings
  }

  def withSettings(settings: WavesSettings)(f: => Unit): Unit = {
    val oldSettings = currentSettings
    currentSettings = settings
    f
    currentSettings = oldSettings
  }

  def withNEmptyBlocksSubscription(count: Int = 2, request: SubscribeRequest = SubscribeRequest.of(1, Int.MaxValue))(
      f: Seq[PBBlockchainUpdated] => Unit
  ): Unit = withGenerateSubscription(request)(d => for (_ <- 1 to count) d.appendBlock())(f)

  def withRepo[T](blocksApi: CommonBlocksApi = stub[CommonBlocksApi], dbOpt: Option[DB] = None)(f: Repo => T): T = {
    val db   = dbOpt.getOrElse(openDB(Files.createTempDirectory("bc-updates").toString))
    val repo = new Repo(db, blocksApi)
    try f(repo)
    finally repo.shutdown()
  }

  trait FakeObserver[T] extends StreamObserver[T] {
    def values: Seq[T]
  }

  object FakeObserver {
    implicit class FakeObserverOps[T](fo: FakeObserver[T]) {
      def fetchUntil(conditionF: Seq[T] => Boolean): Seq[T] = {
        def waitRecTask: Task[Unit] =
          if (conditionF(fo.values)) Task.unit else Task.defer(waitRecTask).delayExecution(50 millis)

        waitRecTask
          .map(_ => fo.values)
          .runSyncUnsafe(1.minute)
      }
    }

    implicit class EventsFakeObserverOps(fo: FakeObserver[SubscribeEvent]) {
      def fetchAllEvents(bc: Blockchain): Seq[SubscribeEvent] =
        fo.fetchUntil(_.exists(_.update.map(_.id.toByteStr) == bc.lastBlockId))

      def lastAppendEvent(bc: Blockchain): BlockAppended =
        fetchAllEvents(bc).last.getUpdate.vanillaAppend
    }
  }

  def createFakeObserver[T](): (FakeObserver[T], CancelableFuture[Seq[T]]) = {
    val promise = Promise[Seq[T]]()
    val obs: CallStreamObserver[T] with FakeObserver[T] = new CallStreamObserver[T] with FakeObserver[T] {
      @volatile var values = Seq.empty[T]

      override def isReady: Boolean                                  = true
      override def setOnReadyHandler(onReadyHandler: Runnable): Unit = ()
      override def disableAutoInboundFlowControl(): Unit             = ()
      override def request(count: Int): Unit                         = ()
      override def setMessageCompression(enable: Boolean): Unit      = ()

      def onNext(value: T): Unit      = values :+= value
      def onError(t: Throwable): Unit = promise.tryFailure(t)
      def onCompleted(): Unit         = promise.trySuccess(values)
    }

    val cancelableFuture = CancelableFuture(promise.future, () => obs.onCompleted())
    (obs, cancelableFuture)
  }

  implicit class UpdatesRepoExt(ur: Repo) {
    def createSubscription(request: SubscribeRequest): CancelableFuture[Seq[SubscribeEvent]] = {
      val (obs, future) = createFakeObserver[SubscribeEvent]()
      ur.subscribe(request, obs)
      future
    }

    // A better way
    def createSubscriptionObserver(request: SubscribeRequest): FakeObserver[SubscribeEvent] = {
      val (obs, _) = createFakeObserver[SubscribeEvent]()
      ur.subscribe(request, obs)
      obs
    }
  }

  // Matchers
  private[this] object E {
    object Block {
      def unapply(bu: PBBlockchainUpdated): Option[(Int, ByteStr)] =
        for {
          a <- bu.update.append
          _ <- a.body.block
        } yield bu.height -> ByteStr(bu.id.toByteArray)
    }

    object Micro {
      def unapply(bu: PBBlockchainUpdated): Option[(Int, ByteStr)] =
        for {
          a <- bu.update.append
          _ <- a.body.microBlock
        } yield bu.height -> ByteStr(bu.id.toByteArray)
    }

    object Rollback {
      def unapply(bu: PBBlockchainUpdated): Option[(Int, ByteStr)] =
        for {
          r <- bu.update.rollback
          if r.`type` == RollbackType.BLOCK
        } yield bu.height -> ByteStr(bu.id.toByteArray)
    }

    object MicroRollback {
      def unapply(bu: PBBlockchainUpdated): Option[(Int, ByteStr)] =
        for {
          r <- bu.update.rollback
          if r.`type` == RollbackType.MICROBLOCK
        } yield bu.height -> ByteStr(bu.id.toByteArray)
    }
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
