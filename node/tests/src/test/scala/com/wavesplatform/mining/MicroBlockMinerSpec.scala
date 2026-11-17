package com.wavesplatform.mining

import com.wavesplatform.TestValues
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.ProtoBlockVersion
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.UtxEvent
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.mining.microblocks.MicroBlockMinerImpl
import com.wavesplatform.network.MicroBlockInv
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.{Blockchain, EndorsementStorage, StateSnapshot}
import com.wavesplatform.test.DomainPresets.RideV6
import com.wavesplatform.test.{FlatSpec, WithResourceManager}
import com.wavesplatform.transaction.TxHelpers.{createAlias, defaultAddress, defaultSigner, secondAddress, transfer}
import com.wavesplatform.transaction.{Transaction, TxVersion}
import com.wavesplatform.utils.Schedulers
import com.wavesplatform.utx.{UtxPool, UtxPoolImpl, UtxPriorityPool}
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import org.scalatest.Inside

import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.concurrent.{Await, Promise}
import scala.concurrent.duration.*
import scala.util.Random

class MicroBlockMinerSpec extends FlatSpec with WithDomain with WithResourceManager with Inside {
  "Micro block miner" should "generate microblocks in flat interval" in {
    val scheduler = Schedulers.singleThread("test")
    val acc       = TestValues.keyPair
    val settings  = domainSettingsWithFS(TestFunctionalitySettings.withFeatures(BlockchainFeatures.NG))
    withDomain(settings, Seq(AddrWithBalance(acc.toAddress, TestValues.bigMoney))) { d =>
      val utxPool = new UtxPoolImpl(ntpTime, d.blockchainUpdater, settings.utxSettings, settings.maxTxErrorLogSize, settings.minerSettings.enable)
      val microBlockMiner = new MicroBlockMinerImpl(
        setDebugState = _ => (),
        allChannels = null,
        d.blockchainUpdater,
        utxPool,
        EndorsementStorage.Disabled,
        settings.minerSettings,
        scheduler,
        scheduler,
        Observable.empty
      )

      def generateBlocks(
          block: Block,
          constraint: MiningConstraint,
          lastMicroBlock: Long
      ): Block = {
        val task = microBlockMiner.generateOneMicroBlockTask(
          acc,
          block,
          constraint,
          lastMicroBlock
        )
        import Scheduler.Implicits.global
        val startTime = System.nanoTime()
        val tx = createAlias(name = "test" + Random.nextInt(), sender = acc, fee = TestValues.fee, version = TxVersion.V1)
        utxPool.putIfNew(tx).resultE.explicitGet()
        val result = task.runSyncUnsafe()
        result match {
          case res @ MicroBlockMinerImpl.Success(b, totalConstraint) =>
            val isFirstBlock = block.transactionData.isEmpty
            val elapsed      = (res.nanoTime - startTime).nanos.toMillis

            if (isFirstBlock) elapsed should be < 1000L
            else elapsed shouldBe settings.minerSettings.microBlockInterval.toMillis +- 1000

            generateBlocks(b, totalConstraint, res.nanoTime)
          case MicroBlockMinerImpl.Stop =>
            d.blockchainUpdater.liquidBlock(d.blockchainUpdater.lastBlockId.get).get
          case MicroBlockMinerImpl.Retry =>
            throw new IllegalStateException()
        }
      }

      val baseBlock = Block
        .buildAndSign(
          3,
          TestValues.timestamp,
          d.lastBlockId,
          d.lastBlock.header.baseTarget,
          d.lastBlock.header.generationSignature,
          txs = Nil,
          acc,
          featureVotes = Nil,
          rewardVote = 0,
          stateHash = None,
          challengedHeader = None,
          finalizationVoting = None
        )
        .explicitGet()

      d.appendBlock(baseBlock)

      val constraint = OneDimensionalMiningConstraint(5, TxEstimators.one, "limit")
      val lastBlock  = generateBlocks(baseBlock, constraint, 0)
      lastBlock.transactionData should have size constraint.rest.toInt
      utxPool.close()
    }
  }

  "Micro block miner" should "retry packing UTX regardless of when event has been sent" in {
    withDomain(RideV6, Seq(AddrWithBalance(defaultAddress, TestValues.bigMoney))) { d =>
      import Scheduler.Implicits.global
      val utxEvents        = ConcurrentSubject.publish[UtxEvent]
      val eventHasBeenSent = new CountDownLatch(1)
      val inner = new UtxPoolImpl(
        ntpTime,
        d.blockchainUpdater,
        RideV6.utxSettings,
        RideV6.maxTxErrorLogSize,
        RideV6.minerSettings.enable,
        { event =>
          utxEvents.onNext(event)
          eventHasBeenSent.countDown()
        }
      )

      val utxPool = new UtxPool {

        override def packUnconfirmed(
            rest: MultiDimensionalMiningConstraint,
            prevStateHash: Option[ByteStr],
            strategy: UtxPool.PackStrategy,
            cancelled: () => Boolean
        ): (Option[Seq[Transaction]], MiningConstraint, Option[ByteStr]) = {
          val (txs, constraint, stateHash) = inner.packUnconfirmed(rest, None, strategy, cancelled)
          val waitingConstraint = new MiningConstraint {
            def isFull: Boolean                                                         = { eventHasBeenSent.await(); constraint.isFull }
            def isOverfilled: Boolean                                                   = constraint.isOverfilled
            def put(b: Blockchain, tx: Transaction, s: StateSnapshot): MiningConstraint = constraint.put(b, tx, s)
          }
          (txs, waitingConstraint, stateHash)
        }

        override def putIfNew(tx: Transaction, forceValidate: Boolean)                = inner.putIfNew(tx, forceValidate)
        override def removeAll(txs: Iterable[Transaction]): Unit                      = inner.removeAll(txs)
        override def all                                                              = inner.all
        override def size                                                             = inner.size
        override def transactionById(transactionId: ByteStr)                          = inner.transactionById(transactionId)
        override def close(): Unit                                                    = inner.close()
        override def scheduleCleanup(): Unit                                          = inner.scheduleCleanup()
        override def setPrioritySnapshots(snapshots: Seq[StateSnapshot]): Unit        = inner.setPrioritySnapshots(snapshots)
        override def addAndScheduleCleanup(transactions: Iterable[Transaction]): Unit = inner.addAndScheduleCleanup(transactions)
        override def resetPriorityPool(): Unit                                        = inner.resetPriorityPool()
        override def cleanUnconfirmed(): Unit                                         = inner.cleanUnconfirmed()
        override def getPriorityPool: Option[UtxPriorityPool]                         = inner.getPriorityPool
      }

      val miner    = Schedulers.singleThread("miner")
      val appender = Schedulers.singleThread("appender")
      val mbminer  = Schedulers.singleThread("micro-block-miner")

      val microBlockMiner = new MicroBlockMinerImpl(
        _ => (),
        null,
        d.blockchainUpdater,
        utxPool,
        EndorsementStorage.Disabled,
        RideV6.minerSettings,
        miner,
        appender,
        utxEvents.collect { case _: UtxEvent.TxAdded => () }
      )

      val block      = d.appendBlock(ProtoBlockVersion)
      val constraint = OneDimensionalMiningConstraint(5, TxEstimators.one, "limit")
      microBlockMiner
        .generateMicroBlockSequence(defaultSigner, block, constraint, 0)
        .runToFuture(using mbminer)

      utxPool.putIfNew(transfer(amount = 123))

      while (d.lastBlockId == block.id()) Thread.sleep(100)
      d.balance(secondAddress) shouldBe 123

      miner.shutdown()
      appender.shutdown()
      mbminer.shutdown()
      inner.close()
    }
  }

  "Micro block miner" should "broadcast an appended microblock even if mining is cancelled right after the append" in withManager { manager =>
    withDomain(RideV6, Seq(AddrWithBalance(defaultAddress, TestValues.bigMoney))) { d =>
      val miner    = manager(Schedulers.singleThread("miner"))
      val appender = manager(Schedulers.singleThread("appender"))
      val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
      val peer     = manager(new EmbeddedChannel())
      channels.add(peer)

      val mining   = Promise[CancelableFuture[?]]()
      val appended = new CountDownLatch(1)
      // UTX cleanup is scheduled on the appender thread right after the microblock is appended: cancel there, like a key block queued behind it
      val utxPool = manager(new UtxPoolImpl(ntpTime, d.blockchainUpdater, RideV6.utxSettings, RideV6.maxTxErrorLogSize, RideV6.minerSettings.enable) {
        override def scheduleCleanup(): Unit = {
          super.scheduleCleanup()
          Await.result(mining.future, 10.seconds).cancel()
          appended.countDown()
        }
      })
      val microBlockMiner = new MicroBlockMinerImpl(
        _ => (),
        channels,
        d.blockchainUpdater,
        utxPool,
        EndorsementStorage.Disabled,
        RideV6.minerSettings,
        miner,
        appender,
        Observable.empty
      )

      val block = d.appendBlock(ProtoBlockVersion)
      utxPool.putIfNew(transfer(amount = 123)).resultE.explicitGet()
      mining.success(microBlockMiner.generateOneMicroBlockTask(defaultSigner, block, MiningConstraint.Unlimited, 0).runToFuture(using miner))

      appended.await(10, TimeUnit.SECONDS) shouldBe true
      // drain both queues so that a broadcast forked after the append has run, if it was not dropped
      Seq(appender, miner).foreach(s => Await.result(Task.evalAsync(()).runToFuture(using s), 10.seconds))

      d.balance(secondAddress) shouldBe 123
      inside(Option(peer.readOutbound[AnyRef]()).value) { case inv: MicroBlockInv =>
        inv.totalBlockId shouldBe d.lastBlockId
        inv.reference shouldBe block.id()
      }
    }
  }
}
