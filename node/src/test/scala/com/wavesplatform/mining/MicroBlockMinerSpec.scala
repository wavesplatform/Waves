package com.wavesplatform.mining

import com.wavesplatform.TestValues
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.ProtoBlockVersion
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.UtxEvent
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.mining.microblocks.MicroBlockMinerImpl
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.{Blockchain, Diff}
import com.wavesplatform.test.DomainPresets.RideV6
import com.wavesplatform.test.FlatSpec
import com.wavesplatform.transaction.TxHelpers.{defaultAddress, defaultSigner, secondAddress, transfer}
import com.wavesplatform.transaction.{CreateAliasTransaction, Transaction, TxVersion}
import com.wavesplatform.utils.Schedulers
import com.wavesplatform.utx.{UtxPool, UtxPoolImpl}
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import org.scalamock.scalatest.PathMockFactory

import java.util.concurrent.CountDownLatch
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.concurrent.duration.Duration.Inf
import scala.util.Random

class MicroBlockMinerSpec extends FlatSpec with PathMockFactory with WithDomain {
  "Micro block miner" should "generate microblocks in flat interval" in {
    val scheduler = Schedulers.singleThread("test")
    val acc       = TestValues.keyPair
    val settings  = domainSettingsWithFS(TestFunctionalitySettings.withFeatures(BlockchainFeatures.NG))
    withDomain(settings, Seq(AddrWithBalance(acc.toAddress, TestValues.bigMoney))) { d =>
      val utxPool = new UtxPoolImpl(ntpTime, d.blockchainUpdater, settings.utxSettings, settings.maxTxErrorLogSize, settings.minerSettings.enable)
      val microBlockMiner = new MicroBlockMinerImpl(
        _ => (),
        null,
        d.blockchainUpdater,
        utxPool,
        settings.minerSettings,
        scheduler,
        scheduler,
        Observable.empty,
        identity
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
        val tx = CreateAliasTransaction
          .selfSigned(TxVersion.V1, acc, "test" + Random.nextInt(), TestValues.fee, TestValues.timestamp)
          .explicitGet()
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
          Nil,
          acc,
          Nil,
          0
        )
        .explicitGet()

      d.appendBlock(baseBlock)

      val constraint = OneDimensionalMiningConstraint(5, TxEstimators.one, "limit")
      val lastBlock  = generateBlocks(baseBlock, constraint, 0)
      lastBlock.transactionData should have size constraint.rest.toInt
    }
  }

  "Micro block miner" should "retry packing UTX regardless of when event has been sent" in {
    withDomain(RideV6, Seq(AddrWithBalance(defaultAddress, TestValues.bigMoney))) { d =>
      import Scheduler.Implicits.global
      val utxEvents        = ConcurrentSubject.publish[UtxEvent]
      val transactionAdded = ConcurrentSubject.replayLimited[Unit](1)

      val utxPool = new UtxPool {
        val eventHasBeenSent = new CountDownLatch(1)
        val inner = new UtxPoolImpl(
          ntpTime,
          d.blockchainUpdater,
          RideV6.utxSettings,
          RideV6.maxTxErrorLogSize,
          RideV6.minerSettings.enable,
          utxEvents.onNext,
          { _ =>
            transactionAdded.onNext(())
            eventHasBeenSent.countDown()
          }
        )

        override def putIfNew(tx: Transaction, forceValidate: Boolean) =
          inner.putIfNew(tx, forceValidate)

        override def packUnconfirmed(
            rest: MultiDimensionalMiningConstraint,
            strategy: UtxPool.PackStrategy,
            cancelled: () => Boolean
        ): (Option[Seq[Transaction]], MiningConstraint) = {
          val (txs, constraint) = inner.packUnconfirmed(rest, strategy, cancelled)
          val waitingConstraint = new MiningConstraint {
            def isFull                                          = { eventHasBeenSent.await(); constraint.isFull }
            def isOverfilled                                    = constraint.isOverfilled
            def put(b: Blockchain, tx: Transaction, diff: Diff) = constraint.put(b, tx, diff)
          }
          (txs, waitingConstraint)
        }

        override def removeAll(txs: Iterable[Transaction]): Unit = ???
        override def all                                         = ???
        override def size                                        = ???
        override def transactionById(transactionId: ByteStr)     = ???
        override def close(): Unit                               = ???
        override def scheduleCleanup(): Unit                     = ???
      }

      val microBlockMiner = new MicroBlockMinerImpl(
        _ => (),
        null,
        d.blockchainUpdater,
        utxPool,
        RideV6.minerSettings,
        Schedulers.singleThread("miner"),
        Schedulers.singleThread("appender"),
        transactionAdded,
        identity
      )

      val block      = d.appendBlock(ProtoBlockVersion)
      val constraint = OneDimensionalMiningConstraint(5, TxEstimators.one, "limit")
      val mined = microBlockMiner
        .generateMicroBlockSequence(defaultSigner, block, constraint, 0)
        .runToFuture(Schedulers.singleThread("micro-block-miner"))

      utxPool.putIfNew(transfer(amount = 123))

      Await.result(mined, Inf)
      d.balance(secondAddress) shouldBe 123
    }
  }
}
