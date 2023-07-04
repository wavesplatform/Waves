package com.wavesplatform.state

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.{Block, ChallengedHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.mining.BlockChallenger
import com.wavesplatform.network.{MessageCodec, PBBlockSpec, PeerDatabase, RawBytes}
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.TxValidationError.GenericError
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.Scheduler

class BlockChallengeTest extends PropSpec with WithDomain {

  val settings = DomainPresets.TransactionStateSnapshot.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  val testTime = TestTime()

  property("NODE-883. Invalid challenging block should be ignored") {
    val sender           = TxHelpers.signer(1)
    val challengedMiner  = TxHelpers.signer(2)
    val challengingMiner = TxHelpers.signer(3)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender)) { d =>
      d.appendBlock()
      val invalidStateHash = ByteStr.fill(DigestLength)(1)
      val txs              = Seq(TxHelpers.transfer(sender, amount = 1), TxHelpers.transfer(sender, amount = 2))
      val challengedBlock  = d.createBlock(Block.ProtoBlockVersion, txs, generator = challengedMiner, stateHash = Some(Some(invalidStateHash)))
      val invalidHashChallengingBlock = createInvalidChallengingBlock(challengingMiner, challengedBlock, Some(Some(invalidStateHash)), d)
      val missedHashChallengingBlock  = createInvalidChallengingBlock(challengingMiner, challengedBlock, Some(None), d)

      d.appendBlockE(invalidHashChallengingBlock) shouldBe Left(GenericError("Invalid block challenge"))
      d.appendBlockE(missedHashChallengingBlock) shouldBe Left(GenericError("Invalid block challenge"))
    }
  }

  property("NODE-885. Consensus data for challenging block should be recalculated") {
    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get
      d.appendBlock(TxHelpers.transfer(TxHelpers.defaultSigner, challengingMiner.toAddress, 1000.waves))
      (1 to 999).foreach(_ => d.appendBlock())
      val invalidStateHash = ByteStr.fill(DigestLength)(1)
      appendAndCheck(d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, stateHash = Some(Some(invalidStateHash))), d) {
        case Some(block) =>
          block.header.challengedHeader shouldBe defined
          val challengedHeader = block.header.challengedHeader.get

          block.header.generator shouldBe challengingMiner.publicKey
          block.header.generator == challengedHeader.generator shouldBe false
          val isConsensusDataEqual =
            block.header.timestamp == challengedHeader.timestamp &&
              block.header.generationSignature == challengedHeader.generationSignature &&
              block.header.baseTarget == challengedHeader.baseTarget

          isConsensusDataEqual shouldBe false
        case _ => fail("block should be defined")
      }
    }
  }

  property("NODE-886. Consensus data for challenging block should be calculated for each mining account from wallet") {
    val challengedMiner = TxHelpers.signer(1)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner, challengedMiner)) { d =>
      val challengingMiner1 = d.wallet.generateNewAccount().get
      val challengingMiner2 = d.wallet.generateNewAccount().get
      d.appendBlock(TxHelpers.transfer(TxHelpers.defaultSigner, challengingMiner1.toAddress, 1001.waves))
      (1 to 999).foreach(_ => d.appendBlock())
      val invalidStateHash = ByteStr.fill(DigestLength)(1)

      appendAndCheck(
        d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, generator = challengedMiner, stateHash = Some(Some(invalidStateHash))),
        d
      ) {
        case Some(block) =>
          block.header.challengedHeader shouldBe defined
          block.header.generator shouldBe challengingMiner1.publicKey
        case _ => fail("block should be defined")
      }

      d.appendBlock(TxHelpers.transfer(challengingMiner1, challengingMiner2.toAddress, 1000.waves))
      (1 to 999).foreach(_ => d.appendBlock())

      appendAndCheck(
        d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, generator = challengedMiner, stateHash = Some(Some(invalidStateHash))),
        d
      ) {
        case Some(block) =>
          block.header.challengedHeader shouldBe defined
          block.header.generator shouldBe challengingMiner2.publicKey
        case _ => fail("block should be defined")
      }
    }
  }

  private def appendAndCheck(block: Block, d: Domain)(check: Option[Block] => Unit): Unit = {
    val channels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    val channel1 = new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp))
    val channel2 = new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp))
    channels.add(channel1)
    channels.add(channel2)
    implicit val appenderScheduler: Scheduler = Scheduler.singleThread("appender")
    val appenderWithChallenger = BlockAppender(
      d.blockchain,
      testTime,
      d.utxPool,
      d.posSelector,
      channels,
      PeerDatabase.NoOp,
      new BlockChallenger(
        d.blockchain,
        channels,
        d.wallet,
        d.settings,
        testTime,
        d.posSelector,
        BlockAppender(d.blockchain, testTime, d.utxPool, d.posSelector, appenderScheduler)
      ),
      appenderScheduler
    )(channel2, _)

    testTime.setTime(block.header.timestamp)
    appenderWithChallenger(block).runSyncUnsafe()
    if (!channel1.outboundMessages().isEmpty)
      check(Some(PBBlockSpec.deserializeData(channel1.readOutbound[RawBytes]().data).get))
    else check(None)
  }

  private def createInvalidChallengingBlock(
      challengingMiner: KeyPair,
      challengedBlock: Block,
      stateHash: Option[Option[ByteStr]],
      d: Domain
  ): Block = {
    d.createBlock(
      Block.ProtoBlockVersion,
      challengedBlock.transactionData,
      generator = challengingMiner,
      stateHash = stateHash,
      challengedHeader = Some(
        ChallengedHeader(
          challengedBlock.header.timestamp,
          challengedBlock.header.baseTarget,
          challengedBlock.header.generationSignature,
          Seq.empty,
          challengedBlock.sender,
          -1,
          challengedBlock.header.stateHash,
          challengedBlock.signature
        )
      )
    )
  }
}
