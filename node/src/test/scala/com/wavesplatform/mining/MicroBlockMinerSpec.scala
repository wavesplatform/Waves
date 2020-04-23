package com.wavesplatform.mining

import cats.effect.concurrent.Ref
import com.wavesplatform.account.Alias
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils._
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.microblocks.MicroBlockMinerImpl
import com.wavesplatform.mining.microblocks.MicroBlockMinerImpl.MicroBlockMiningResult
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.transaction.{CreateAliasTransaction, GenesisTransaction, TxVersion}
import com.wavesplatform.utils.Schedulers
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.{TestValues, TransactionGen}
import monix.eval.Task
import monix.execution.Scheduler
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}

import scala.concurrent.duration._
import scala.util.Random

class MicroBlockMinerSpec extends FlatSpec with Matchers with PrivateMethodTester with PathMockFactory with WithDomain with TransactionGen {
  "Micro block miner" should "generate microblocks in flat interval" in {
    val scheduler = Schedulers.singleThread("test")
    val acc       = TestValues.keyPair
    val genesis   = GenesisTransaction.create(acc, TestValues.bigMoney, TestValues.timestamp).explicitGet()
    val settings  = domainSettingsWithFS(TestFunctionalitySettings.withFeatures(BlockchainFeatures.NG))
    withDomain(settings) { d =>
      d.appendBlock(TestBlock.create(Seq(genesis)))
      val utxPool = new UtxPoolImpl(ntpTime, d.blockchainUpdater, ignoreSpendableBalanceChanged, settings.utxSettings, enablePriorityPool = true)
      val microBlockMiner = new MicroBlockMinerImpl(
        Ref.unsafe[Task, MinerDebugInfo.State](MinerDebugInfo.Disabled),
        null,
        d.blockchainUpdater,
        utxPool,
        settings.minerSettings,
        scheduler,
        scheduler
      )
      val generateOneMicroBlockTask = PrivateMethod[Task[MicroBlockMiningResult]]('generateOneMicroBlockTask)

      def generateBlocks(
          block: Block,
          constraint: MiningConstraint
      ): Block = {
        val task = microBlockMiner invokePrivate generateOneMicroBlockTask(
          acc,
          block,
          MiningConstraints(d.blockchainUpdater, d.blockchainUpdater.height, Some(settings.minerSettings)),
          constraint
        )
        import Scheduler.Implicits.global
        val startTime = System.nanoTime()
        val tx = CreateAliasTransaction
          .selfSigned(TxVersion.V1, acc, Alias.create("test" + Random.nextInt()).explicitGet(), TestValues.fee, TestValues.timestamp)
          .explicitGet()
        utxPool.putIfNew(tx).resultE shouldBe 'right
        val result = task.runSyncUnsafe()
        result match {
          case MicroBlockMinerImpl.Success(b, totalConstraint) =>
            (System.nanoTime() - startTime).nanos.toMillis shouldBe settings.minerSettings.microBlockInterval.toMillis +- 1000
            generateBlocks(b, totalConstraint)
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
      val lastBlock = generateBlocks(baseBlock, constraint)
      lastBlock.transactionData should have size constraint.rest.toInt
    }
    }
}
