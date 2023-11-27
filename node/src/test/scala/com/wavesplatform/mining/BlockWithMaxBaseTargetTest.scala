package com.wavesplatform.mining

import java.security.Permission
import java.util.concurrent.{Semaphore, TimeUnit}

import com.typesafe.config.ConfigFactory
import com.wavesplatform.WithNewDBForEachTest
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.db.DBCacheSettings
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.BlockWithMaxBaseTargetTest.Env
import com.wavesplatform.settings.*
import com.wavesplatform.state.*
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.utils.TestRocksDB
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.{BlockchainUpdater, GenesisTransaction}
import com.wavesplatform.utils.BaseTargetReachedMaximum
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import monix.reactive.Observable
import org.scalacheck.{Arbitrary, Gen}

import scala.concurrent.Await
import scala.concurrent.duration.*

class BlockWithMaxBaseTargetTest extends FreeSpec with WithNewDBForEachTest with DBCacheSettings {

  "base target limit" - {
    "node should stop if base target greater than maximum in block creation " in {
      withEnv { case Env(settings, pos, bcu, utxPoolStub, scheduler, account, lastBlock) =>
        var stopReasonCode = 0

        val allChannels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
        val wallet      = Wallet(WalletSettings(None, Some("123"), None))
        val miner =
          new MinerImpl(allChannels, bcu, settings, ntpTime, utxPoolStub, wallet, pos, scheduler, scheduler, Observable.empty)

        val signal = new Semaphore(1)
        signal.acquire()

        System.setSecurityManager(new SecurityManager {
          override def checkPermission(perm: Permission): Unit = {}

          override def checkPermission(perm: Permission, context: Object): Unit = {}

          override def checkExit(status: Int): Unit = signal.synchronized {
            super.checkExit(status)
            stopReasonCode = status
            if (status == BaseTargetReachedMaximum.code)
              signal.release()
            throw new SecurityException("System exit is not allowed")
          }
        })

        try {
          miner.forgeBlock(account)
        } catch {
          case _: SecurityException => // NOP
        }

        signal.tryAcquire(10, TimeUnit.SECONDS)

        stopReasonCode shouldBe BaseTargetReachedMaximum.code

        System.setSecurityManager(null)
      }
    }

    "node should stop if base target greater than maximum in block append" in {
      withEnv { case Env(settings, pos, bcu, utxPoolStub, scheduler, _, lastBlock) =>
        var stopReasonCode = 0

        val signal = new Semaphore(1)
        signal.acquire()

        System.setSecurityManager(new SecurityManager {
          override def checkPermission(perm: Permission): Unit = {}

          override def checkPermission(perm: Permission, context: Object): Unit = {}

          override def checkExit(status: Int): Unit = signal.synchronized {
            super.checkExit(status)
            stopReasonCode = status
            if (status == BaseTargetReachedMaximum.code)
              signal.release()
            throw new SecurityException("System exit is not allowed")
          }
        })

        val blockAppendTask = BlockAppender(bcu, ntpTime, utxPoolStub, pos, scheduler)(lastBlock, None).onErrorRecoverWith[Any] {
          case _: SecurityException =>
            Task.unit
        }
        Await.result(blockAppendTask.runToFuture(scheduler), 1.minute)

        signal.tryAcquire(10, TimeUnit.SECONDS)

        stopReasonCode shouldBe BaseTargetReachedMaximum.code

        System.setSecurityManager(null)
      }
    }
  }

  def withEnv(f: Env => Unit): Unit = {
    val defaultWriter = TestRocksDB.withFunctionalitySettings(db, TestFunctionalitySettings.Stub)

    val settings0     = WavesSettings.fromRootConfig(loadConfig(ConfigFactory.load()))
    val minerSettings = settings0.minerSettings.copy(quorum = 0)
    val blockchainSettings0 = settings0.blockchainSettings.copy(
      functionalitySettings = settings0.blockchainSettings.functionalitySettings.copy(preActivatedFeatures = Map(BlockchainFeatures.FairPoS.id -> 1))
    )
    val synchronizationSettings0 = settings0.synchronizationSettings.copy(maxBaseTarget = Some(1L))
    val settings = settings0.copy(
      blockchainSettings = blockchainSettings0,
      minerSettings = minerSettings,
      synchronizationSettings = synchronizationSettings0,
      featuresSettings = settings0.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false)
    )

    val bcu =
      new BlockchainUpdaterImpl(defaultWriter, settings, ntpTime, ignoreBlockchainUpdateTriggers, (_, _) => Seq.empty)
    val pos = PoSSelector(bcu, settings.synchronizationSettings.maxBaseTarget)

    val utxPoolStub = new UtxPoolImpl(ntpTime, bcu, settings0.utxSettings, settings.maxTxErrorLogSize, settings0.minerSettings.enable)
    val schedulerService: SchedulerService = Scheduler.singleThread("appender")

    try {

      val ts = ntpTime.correctedTime() - 60000
      val (account, firstBlock, secondBlock) =
        Gen
          .containerOfN[Array, Byte](32, Arbitrary.arbitrary[Byte])
          .map(bs => KeyPair(bs))
          .map { account =>
            val tx           = GenesisTransaction.create(account.toAddress, ENOUGH_AMT, ts + 1).explicitGet()
            val genesisBlock = TestBlock.create(ts + 2, List(tx)).block
            val secondBlock = TestBlock
              .create(
                ts + 3,
                genesisBlock.id(),
                Seq.empty,
                account
              )
              .block
            (account, genesisBlock, secondBlock)
          }
          .sample
          .get

      bcu.processBlock(firstBlock, firstBlock.header.generationSignature, None).explicitGet()

      f(Env(settings, pos, bcu, utxPoolStub, schedulerService, account, secondBlock))
    } finally {
      schedulerService.shutdown()
      utxPoolStub.close()
      bcu.shutdown()
    }
  }
}

object BlockWithMaxBaseTargetTest {

  final case class Env(
      settings: WavesSettings,
      pos: PoSSelector,
      bcu: Blockchain with BlockchainUpdater with NG,
      utxPool: UtxPoolImpl,
      schedulerService: SchedulerService,
      miner: KeyPair,
      lastBlock: Block
  )
}
