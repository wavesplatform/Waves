package com.wavesplatform.state

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.{Block, ChallengedHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.BlockChallenger
import com.wavesplatform.network.{MessageCodec, PBBlockSpec, PeerDatabase, RawBytes}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.transaction.{Transaction, TxHelpers}
import com.wavesplatform.transaction.TxValidationError.{BlockAppendError, GenericError, MicroBlockAppendError}
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.eval.Task
import monix.execution.Scheduler

class BlockChallengeTest extends PropSpec with WithDomain {

  implicit val appenderScheduler: Scheduler = Scheduler.singleThread("appender")
  val settings: WavesSettings               = DomainPresets.TransactionStateSnapshot.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  val testTime: TestTime                    = TestTime()

//  property("NODE-883. Invalid challenging block should be ignored") {
//    val sender           = TxHelpers.signer(1)
//    val challengedMiner  = TxHelpers.signer(2)
//    val challengingMiner = TxHelpers.signer(3)
//    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender)) { d =>
//      d.appendBlock()
//      val invalidStateHash = ByteStr.fill(DigestLength)(1)
//      val txs              = Seq(TxHelpers.transfer(sender, amount = 1), TxHelpers.transfer(sender, amount = 2))
//      val challengedBlock  = d.createBlock(Block.ProtoBlockVersion, txs, generator = challengedMiner, stateHash = Some(Some(invalidStateHash)))
//      val invalidHashChallengingBlock = createChallengingBlock(d, challengingMiner, challengedBlock, Some(Some(invalidStateHash)))
//      val missedHashChallengingBlock  = createChallengingBlock(d, challengingMiner, challengedBlock, Some(None))
//
//      d.appendBlockE(invalidHashChallengingBlock) shouldBe Left(GenericError("Invalid block challenge"))
//      d.appendBlockE(missedHashChallengingBlock) shouldBe Left(GenericError("Invalid block challenge"))
//    }
//  }
//
//  property("NODE-885. Consensus data for challenging block should be recalculated") {
//    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
//      val challengingMiner = d.wallet.generateNewAccount().get
//      d.appendBlock(TxHelpers.transfer(TxHelpers.defaultSigner, challengingMiner.toAddress, 1000.waves))
//      (1 to 999).foreach(_ => d.appendBlock())
//      val invalidStateHash = ByteStr.fill(DigestLength)(1)
//      appendAndCheck(d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, stateHash = Some(Some(invalidStateHash))), d) {
//        case Some(block) =>
//          block.header.challengedHeader shouldBe defined
//          val challengedHeader = block.header.challengedHeader.get
//
//          block.header.generator shouldBe challengingMiner.publicKey
//          block.header.generator == challengedHeader.generator shouldBe false
//          val isConsensusDataEqual =
//            block.header.timestamp == challengedHeader.timestamp &&
//              block.header.generationSignature == challengedHeader.generationSignature &&
//              block.header.baseTarget == challengedHeader.baseTarget
//
//          isConsensusDataEqual shouldBe false
//        case _ => fail("block should be defined")
//      }
//    }
//  }
//
//  property("NODE-886. Consensus data for challenging block should be calculated for each mining account from wallet") {
//    val challengedMiner = TxHelpers.signer(1)
//    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner, challengedMiner)) { d =>
//      val challengingMiner1 = d.wallet.generateNewAccount().get
//      val challengingMiner2 = d.wallet.generateNewAccount().get
//      d.appendBlock(TxHelpers.transfer(TxHelpers.defaultSigner, challengingMiner1.toAddress, 1001.waves))
//      (1 to 999).foreach(_ => d.appendBlock())
//      val invalidStateHash = ByteStr.fill(DigestLength)(1)
//
//      appendAndCheck(
//        d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, generator = challengedMiner, stateHash = Some(Some(invalidStateHash))),
//        d
//      ) {
//        case Some(block) =>
//          block.header.challengedHeader shouldBe defined
//          block.header.generator shouldBe challengingMiner1.publicKey
//        case _ => fail("block should be defined")
//      }
//
//      d.appendBlock(TxHelpers.transfer(challengingMiner1, challengingMiner2.toAddress, 1000.waves))
//      (1 to 999).foreach(_ => d.appendBlock())
//
//      appendAndCheck(
//        d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, generator = challengedMiner, stateHash = Some(Some(invalidStateHash))),
//        d
//      ) {
//        case Some(block) =>
//          block.header.challengedHeader shouldBe defined
//          block.header.generator shouldBe challengingMiner2.publicKey
//        case _ => fail("block should be defined")
//      }
//    }
//  }
//
//  property("NODE-888. ChallengedHeader should contain info from original block header") {
//    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
//      val challengingMiner = d.wallet.generateNewAccount().get
//      d.appendBlock(TxHelpers.transfer(TxHelpers.defaultSigner, challengingMiner.toAddress, 1000.waves))
//      (1 to 999).foreach(_ => d.appendBlock())
//      val invalidStateHash = ByteStr.fill(DigestLength)(1)
//      val originalBlock    = d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, stateHash = Some(Some(invalidStateHash)))
//      appendAndCheck(originalBlock, d) {
//        case Some(block) =>
//          block.header.challengedHeader shouldBe defined
//          val challengedHeader = block.header.challengedHeader.get
//
//          challengedHeader.timestamp shouldBe originalBlock.header.timestamp
//          challengedHeader.baseTarget shouldBe originalBlock.header.baseTarget
//          challengedHeader.generationSignature shouldBe originalBlock.header.generationSignature
//          challengedHeader.featureVotes shouldBe originalBlock.header.featureVotes
//          challengedHeader.generator shouldBe originalBlock.header.generator
//          challengedHeader.rewardVote shouldBe originalBlock.header.rewardVote
//          challengedHeader.stateHash shouldBe originalBlock.header.stateHash
//          challengedHeader.headerSignature shouldBe originalBlock.signature
//        case _ => fail("block should be defined")
//      }
//    }
//  }
//
//  property(
//    s"NODE-889. Block without challenge (before ${BlockchainFeatures.TransactionStateSnapshot} activation) should not contain ChallengedHeader"
//  ) {
//    val sender           = TxHelpers.signer(1)
//    val challengedMiner  = TxHelpers.signer(2)
//    val challengingMiner = TxHelpers.signer(3)
//    withDomain(DomainPresets.ConsensusImprovements, balances = AddrWithBalance.enoughBalances(sender, challengedMiner, challengingMiner)) { d =>
//      d.appendBlock()
//      val invalidStateHash = ByteStr.fill(DigestLength)(1)
//      val txs              = Seq(TxHelpers.transfer(sender, amount = 1), TxHelpers.transfer(sender, amount = 2))
//      val challengedBlock =
//        d.createBlock(Block.ProtoBlockVersion, txs, strictTime = true, generator = challengedMiner, stateHash = Some(Some(invalidStateHash)))
//      val blockWithChallenge = createChallengingBlock(d, challengingMiner, challengedBlock, Some(Some(invalidStateHash)))
//
//      testTime.setTime(blockWithChallenge.header.timestamp.max(challengedBlock.header.timestamp))
//      createBlockAppender(d)(blockWithChallenge).runSyncUnsafe() shouldBe Left(
//        BlockAppendError("Challenged header is not supported yet", blockWithChallenge)
//      )
//    }
//  }
//
//  property("NODE-890. Challenging block should contain all transactions from original block") {
//    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
//      val challengingMiner = d.wallet.generateNewAccount().get
//      d.appendBlock(TxHelpers.transfer(TxHelpers.defaultSigner, challengingMiner.toAddress, 1000.waves))
//      (1 to 999).foreach(_ => d.appendBlock())
//      val invalidStateHash = ByteStr.fill(DigestLength)(1)
//      val originalBlock    = d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, stateHash = Some(Some(invalidStateHash)))
//      appendAndCheck(originalBlock, d) {
//        case Some(block) =>
//          block.transactionData shouldBe originalBlock.transactionData
//        case _ => fail("block should be defined")
//      }
//    }
//  }
//
//  property("NODE-891. Challenging block should contain only transactions from original block") {
//    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
//      val challengingMiner = d.wallet.generateNewAccount().get
//      d.appendBlock(TxHelpers.transfer(TxHelpers.defaultSigner, challengingMiner.toAddress, 1000.waves))
//      (1 to 999).foreach(_ => d.appendBlock())
//      val invalidStateHash = ByteStr.fill(DigestLength)(1)
//      val originalBlock    = d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, stateHash = Some(Some(invalidStateHash)))
//      val invalidChallengingBlock = createChallengingBlock(
//        d,
//        challengingMiner,
//        originalBlock,
//        None,
//        txs = Some(originalBlock.transactionData :+ TxHelpers.transfer(TxHelpers.defaultSigner))
//      )
//
//      d.appendBlockE(invalidChallengingBlock) shouldBe Left(GenericError(s"Block $invalidChallengingBlock has invalid signature"))
//
//      val correctChallengingBlock = createChallengingBlock(d, challengingMiner, originalBlock, None)
//      d.appendBlockE(correctChallengingBlock) should beRight
//
//      val microblock = d.createMicroBlock(None, Some(challengingMiner), TxHelpers.transfer(TxHelpers.defaultSigner))
//      d.appendMicroBlockE(microblock) shouldBe Left(MicroBlockAppendError("Base block has challenged header", microblock))
//    }
//  }
//
//  property("NODE-892. Challenging block should reference the same block as original") {
//    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
//      val challengingMiner = d.wallet.generateNewAccount().get
//      d.appendBlock(TxHelpers.transfer(TxHelpers.defaultSigner, challengingMiner.toAddress, 1000.waves))
//      (1 to 999).foreach(_ => d.appendBlock())
//      val invalidStateHash = ByteStr.fill(DigestLength)(1)
//      val originalBlock    = d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, stateHash = Some(Some(invalidStateHash)))
//      appendAndCheck(originalBlock, d) {
//        case Some(block) =>
//          block.header.reference shouldBe originalBlock.header.reference
//        case _ => fail("block should be defined")
//      }
//    }
//  }
//
//  property("NODE-893. Challenging block can't reference blocks before previous") {
//    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
//      val challengingMiner = d.wallet.generateNewAccount().get
//      d.appendBlock(TxHelpers.transfer(TxHelpers.defaultSigner, challengingMiner.toAddress, 1000.waves))
//      (1 to 999).foreach(_ => d.appendBlock())
//      val invalidStateHash = ByteStr.fill(DigestLength)(1)
//      val originalBlock    = d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, stateHash = Some(Some(invalidStateHash)))
//      val grandParent      = d.blockchain.lastBlockHeader.map(_.header.reference)
//      val challengingBlock = createChallengingBlock(d, challengingMiner, originalBlock, None, grandParent)
//
//      d.appendBlockE(challengingBlock) shouldBe Left(BlockAppendError("References incorrect or non-existing block", challengingBlock))
//    }
//  }
//
//  property("NODE-898. Block reward and fees should be distributed to challenging miner") {
//    val sender = TxHelpers.signer(1)
//    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender)) { d =>
//      val challengingMiner = d.wallet.generateNewAccount().get
//
//      d.appendBlock(TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves))
//
//      (1 to 999).foreach(_ => d.appendBlock())
//      val prevBlockTx = TxHelpers.transfer(sender)
//
//      d.appendBlock(prevBlockTx)
//
//      val invalidStateHash   = ByteStr.fill(DigestLength)(1)
//      val challengedBlockTxs = Seq(TxHelpers.transfer(sender), TxHelpers.transfer(sender))
//      val originalBlock      = d.createBlock(Block.ProtoBlockVersion, challengedBlockTxs, strictTime = true, stateHash = Some(Some(invalidStateHash)))
//      val originalMinerBalance    = d.balance(originalBlock.header.generator.toAddress)
//      val challengingMinerBalance = d.balance(challengingMiner.toAddress)
//      val challengingBlock        = createChallengingBlock(d, challengingMiner, originalBlock)
//
//      d.appendBlockE(challengingBlock) should beRight
//
//      d.balance(originalBlock.header.generator.toAddress) shouldBe originalMinerBalance
//      d.balance(
//        challengingMiner.toAddress
//      ) shouldBe challengingMinerBalance + d.settings.blockchainSettings.rewardsSettings.initial + (prevBlockTx.fee.value - BlockDiffer
//        .CurrentBlockFeePart(prevBlockTx.fee.value)) + challengedBlockTxs.map(tx => BlockDiffer.CurrentBlockFeePart(tx.fee.value)).sum
//    }
//  }
//
//  property("NODE-901. Elided transaction sender should not pay fee") {
//    val sender          = TxHelpers.signer(1)
//    val challengedMiner = TxHelpers.signer(2)
//    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender)) { d =>
//      val challengingMiner = d.wallet.generateNewAccount().get
//
//      d.appendBlock(
//        TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves),
//        TxHelpers.transfer(sender, challengedMiner.toAddress, 1000.waves)
//      )
//
//      (1 to 999).foreach(_ => d.appendBlock())
//
//      val invalidStateHash  = ByteStr.fill(DigestLength)(1)
//      val challengedBlockTx = TxHelpers.transfer(challengedMiner, amount = 1005.waves)
//      val originalBlock = d.createBlock(
//        Block.ProtoBlockVersion,
//        Seq(challengedBlockTx),
//        strictTime = true,
//        generator = challengedMiner,
//        stateHash = Some(Some(invalidStateHash))
//      )
//      val challengingBlock = createChallengingBlock(d, challengingMiner, originalBlock)
//
//      val elidedTxSenderBalance = d.balance(challengedMiner.toAddress)
//
//      d.appendBlockE(challengingBlock) should beRight
//
//      d.transactionsApi.transactionById(challengedBlockTx.id()).map(_.status).contains(TxMeta.Status.Elided) shouldBe true
//      d.balance(challengedMiner.toAddress) shouldBe elidedTxSenderBalance
//    }
//  }

  property("NODE-902. Elided transaction should have unique ID") {
    val sender          = TxHelpers.signer(1)
    val challengedMiner = TxHelpers.signer(2)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves),
        TxHelpers.transfer(sender, challengedMiner.toAddress, 1000.waves)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val invalidStateHash  = ByteStr.fill(DigestLength)(1)
      val challengedBlockTx = TxHelpers.transfer(challengedMiner, amount = 1005.waves)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq(challengedBlockTx),
        strictTime = true,
        generator = challengedMiner,
        stateHash = Some(Some(invalidStateHash))
      )

      appendAndCheck(originalBlock, d) {
        case Some(block) =>
          d.transactionsApi.transactionById(challengedBlockTx.id()).map(_.status).contains(TxMeta.Status.Elided) shouldBe true
          block.transactionData.head.id() shouldBe challengedBlockTx.id()

          d.appendBlock(TxHelpers.transfer(sender, challengedMiner.toAddress, 1000.waves))
          println(d.appendBlockE(challengedBlockTx))
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
        createBlockAppender(d)
      ),
      appenderScheduler
    )(channel2, _)

    testTime.setTime(block.header.timestamp)
    appenderWithChallenger(block).runSyncUnsafe()
    if (!channel1.outboundMessages().isEmpty)
      check(Some(PBBlockSpec.deserializeData(channel1.readOutbound[RawBytes]().data).get))
    else check(None)
  }

  private def createBlockAppender(d: Domain): Block => Task[Either[ValidationError, Option[BigInt]]] =
    BlockAppender(d.blockchain, testTime, d.utxPool, d.posSelector, appenderScheduler)

  private def createChallengingBlock(
      d: Domain,
      challengingMiner: KeyPair,
      challengedBlock: Block,
      stateHash: Option[Option[ByteStr]] = None,
      ref: Option[ByteStr] = None,
      txs: Option[Seq[Transaction]] = None
  ): Block = {
    d.createBlock(
      Block.ProtoBlockVersion,
      txs.getOrElse(challengedBlock.transactionData),
      ref.orElse(d.blockchain.lastBlockId),
      strictTime = true,
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
