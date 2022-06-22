package com.wavesplatform.mining

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.{WalletSettings, WavesSettings}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.{TxHelpers, TxVersion}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.Scheduler
import monix.reactive.Observable
import DomainPresets.*
import com.wavesplatform.block.Block
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.appender.BlockAppender
import monix.eval.Task

import scala.concurrent.duration.*

class MinerAccountScriptRestrictionsTest extends PropSpec with WithDomain {

  type Appender = Block => Task[Either[ValidationError, Option[BigInt]]]

  val time: TestTime    = TestTime()
  val minerAcc: KeyPair = TxHelpers.signer(1)

  property("miner account can have any script after RideV6 feature activation") {
    Seq(
      dAppScriptWithVerifier(true),
      dAppScriptWithVerifier(false),
      dAppScriptWithoutVerifier,
      accountScript(true),
      accountScript(false)
    ).foreach { script =>
      withDomain(
        DomainPresets.RideV5.setFeaturesHeight((BlockchainFeatures.RideV6, 3)),
        AddrWithBalance.enoughBalances(minerAcc)
      ) { d =>
        withMiner(d) { (miner, appender, scheduler) =>
          d.appendBlock(setScript(script))
          miner.getNextBlockGenerationOffset(minerAcc) should produce(errMsgBeforeRideV6)
          forgeAndAppendBlock(d, miner, appender)(scheduler) should produce(errMsgBeforeRideV6)

          d.appendBlock()
          miner.getNextBlockGenerationOffset(minerAcc) should beRight
          forgeAndAppendBlock(d, miner, appender)(scheduler) should beRight
        }
      }
    }
  }

  private def errMsgBeforeRideV6 =
    s"Account(${minerAcc.toAddress}) is scripted and not allowed to forge blocks"

  private def ts: Long = System.currentTimeMillis()

  private def withMiner(d: Domain)(f: (MinerImpl, Appender, Scheduler) => Unit): Unit = {
    val defaultSettings = WavesSettings.default()
    val wavesSettings   = defaultSettings.copy(minerSettings = defaultSettings.minerSettings.copy(quorum = 0))

    val utx = new UtxPoolImpl(time, d.blockchainUpdater, wavesSettings.utxSettings, isMiningEnabled = wavesSettings.minerSettings.enable)
    val appenderScheduler = Scheduler.singleThread("appender")

    val miner = new MinerImpl(
      new DefaultChannelGroup(GlobalEventExecutor.INSTANCE),
      d.blockchainUpdater,
      wavesSettings,
      time,
      utx,
      Wallet(WalletSettings(None, Some("123"), Some(ByteStr(minerAcc.seed)))),
      d.posSelector,
      Scheduler.singleThread("miner"),
      appenderScheduler,
      Observable.empty
    )

    val appender = BlockAppender(d.blockchainUpdater, time, utx, d.posSelector, appenderScheduler) _

    f(miner, appender, appenderScheduler)
  }

  private def forgeAndAppendBlock(d: Domain, miner: MinerImpl, appender: Appender)(implicit scheduler: Scheduler) = {
    time.setTime(
      d.lastBlock.header.timestamp + d.posSelector
        .getValidBlockDelay(d.blockchain.height, minerAcc, d.lastBlock.header.baseTarget, d.blockchain.generatingBalance(minerAcc.toAddress))
        .explicitGet()
    )
    val forge = miner.forgeBlock(minerAcc)
    val block = forge.explicitGet()._1
    appender(block).runSyncUnsafe(10.seconds)
  }

  private def setScript(script: Script): SetScriptTransaction =
    SetScriptTransaction.selfSigned(TxVersion.V2, minerAcc, Some(script), 0.01.waves, ts).explicitGet()

  private def accountScript(result: Boolean): ExprScript =
    TestCompiler(V5).compileExpression(result.toString)

  private def dAppScriptWithVerifier(result: Boolean): ContractScriptImpl = {
    val expr =
      s"""
         |@Callable(i)
         |func c() = []
         |
         |@Verifier(tx)
         |func v() = $result
         |""".stripMargin
    TestCompiler(V5).compileContract(expr)
  }

  private def dAppScriptWithoutVerifier: ContractScriptImpl = {
    val expr =
      """
        |@Callable(i)
        |func c() = []
        |""".stripMargin
    TestCompiler(V5).compileContract(expr)
  }
}
