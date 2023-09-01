package com.wavesplatform.state

import akka.http.scaladsl.model.{ContentTypes, FormData, HttpEntity}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.*
import com.wavesplatform.TestValues
import com.wavesplatform.account.{Address, KeyPair, SeedKeyPair}
import com.wavesplatform.api.http.TransactionsApiRoute.{ApplicationStatus, Status}
import com.wavesplatform.api.http.*
import com.wavesplatform.block.{Block, ChallengedHeader, MicroBlock}
import com.wavesplatform.common.merkle.Merkle
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.{Domain, defaultSigner}
import com.wavesplatform.http.DummyTransactionPublisher
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, CONST_LONG}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.mining.{BlockChallenger, BlockChallengerImpl}
import com.wavesplatform.network.MicroBlockSynchronizer.MicroblockData
import com.wavesplatform.network.{ExtensionBlocks, InvalidBlockStorage, MessageCodec, PBBlockSpec, PeerDatabase, RawBytes}
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.BlockRewardCalculator.BlockRewardShares
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult
import com.wavesplatform.state.appender.{BlockAppender, ExtensionAppender, MicroblockAppender}
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.state.diffs.BlockDiffer.CurrentBlockFeePart
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxValidationError.{BlockAppendError, GenericError, InvalidStateHash, MicroBlockAppendError}
import com.wavesplatform.transaction.assets.exchange.OrderType
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.utils.EthConverters.*
import com.wavesplatform.transaction.{EthTxGenerator, Transaction, TxHelpers, TxNonNegativeAmount, TxVersion}
import com.wavesplatform.utils.{JsonMatchers, Schedulers, SharedSchedulerMixin}
import io.netty.channel.Channel
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.group.{ChannelGroup, DefaultChannelGroup}
import io.netty.util.HashedWheelTimer
import io.netty.util.concurrent.GlobalEventExecutor
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import org.scalatest.Assertion
import play.api.libs.json.*

import java.util.concurrent.locks.ReentrantLock
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Promise}

class BlockChallengeTest extends PropSpec with WithDomain with ScalatestRouteTest with ApiMarshallers with JsonMatchers with SharedSchedulerMixin {

  implicit val appenderScheduler: Scheduler = Scheduler.singleThread("appender")
  val settings: WavesSettings =
    DomainPresets.TransactionStateSnapshot.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  val testTime: TestTime = TestTime()

  val invalidStateHash: ByteStr = ByteStr.fill(DigestLength)(1)

  property("NODE-883. Invalid challenging block should be ignored") {
    val sender           = TxHelpers.signer(1)
    val challengedMiner  = TxHelpers.signer(2)
    val challengingMiner = TxHelpers.signer(3)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender)) { d =>
      d.appendBlock()
      val txs             = Seq(TxHelpers.transfer(sender, amount = 1), TxHelpers.transfer(sender, amount = 2))
      val challengedBlock = d.createBlock(Block.ProtoBlockVersion, txs, generator = challengedMiner, stateHash = Some(Some(invalidStateHash)))
      val invalidHashChallengingBlock = d.createChallengingBlock(challengingMiner, challengedBlock, stateHash = Some(Some(invalidStateHash)))
      val missedHashChallengingBlock  = d.createChallengingBlock(challengingMiner, challengedBlock, stateHash = Some(None))

      d.appendBlockE(invalidHashChallengingBlock) shouldBe Left(InvalidStateHash(Some(invalidStateHash)))
      d.appendBlockE(missedHashChallengingBlock) shouldBe Left(InvalidStateHash(None))
    }
  }

  property("NODE-884. Challenging miner should have correct balances") {
    val challengedMiner = TxHelpers.signer(1)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get
      d.appendBlock(
        TxHelpers.transfer(TxHelpers.defaultSigner, challengingMiner.toAddress, 1000.waves),
        TxHelpers.transfer(TxHelpers.defaultSigner, challengedMiner.toAddress, 2000.waves)
      )
      (1 to 999).foreach(_ => d.appendBlock())
      val originalBlock =
        d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, generator = challengedMiner, stateHash = Some(Some(invalidStateHash)))
      val challengingBlock = d.createChallengingBlock(challengingMiner, originalBlock)

      val challengingGenBalanceBefore = d.blockchain.generatingBalance(challengingMiner.toAddress, Some(challengingBlock.header.reference))
      val challengingEffBalanceBefore = d.blockchain.effectiveBalance(challengingMiner.toAddress, 0)
      val challengedGenBalanceBefore  = d.blockchain.generatingBalance(challengedMiner.toAddress, Some(challengingBlock.header.reference))

      d.appendBlockE(challengingBlock) should beRight
      d.blockchain.generatingBalance(
        challengingMiner.toAddress,
        Some(challengingBlock.header.reference)
      ) shouldBe challengingGenBalanceBefore + challengedGenBalanceBefore

      val minerReward = getLastBlockMinerReward(d)
      d.blockchain.effectiveBalance(
        challengingMiner.toAddress,
        0
      ) shouldBe challengingEffBalanceBefore + minerReward

      d.blockchain.generatingBalance(challengingMiner.toAddress, Some(challengingBlock.id())) shouldBe challengingGenBalanceBefore
    }
  }

  property("NODE-885. Consensus data for challenging block should be recalculated") {
    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get
      d.appendBlock(TxHelpers.transfer(TxHelpers.defaultSigner, challengingMiner.toAddress, 1000.waves))
      (1 to 999).foreach(_ => d.appendBlock())
      appendAndCheck(d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, stateHash = Some(Some(invalidStateHash))), d) { block =>
        block.header.challengedHeader shouldBe defined
        val challengedHeader = block.header.challengedHeader.get

        block.header.generator shouldBe challengingMiner.publicKey
        block.header.generator == challengedHeader.generator shouldBe false
        val isConsensusDataEqual =
          block.header.timestamp == challengedHeader.timestamp &&
            block.header.generationSignature == challengedHeader.generationSignature &&
            block.header.baseTarget == challengedHeader.baseTarget

        isConsensusDataEqual shouldBe false
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

      appendAndCheck(
        d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, generator = challengedMiner, stateHash = Some(Some(invalidStateHash))),
        d
      ) { block =>
        block.header.challengedHeader shouldBe defined
        block.header.generator shouldBe challengingMiner1.publicKey
      }

      d.appendBlock(TxHelpers.transfer(challengingMiner1, challengingMiner2.toAddress, 1000.waves))
      (1 to 999).foreach(_ => d.appendBlock())

      appendAndCheck(
        d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, generator = challengedMiner, stateHash = Some(Some(invalidStateHash))),
        d
      ) { block =>
        block.header.challengedHeader shouldBe defined
        block.header.generator shouldBe challengingMiner2.publicKey
      }
    }
  }

  property("NODE-887. BlockChallenger should pick account with the best timestamp") {
    val challengedMiner = TxHelpers.signer(1)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
      val challenger      = createBlockChallenger(d)
      val accNum          = 10
      val challengingAccs = (1 to accNum).flatMap(_ => d.wallet.generateNewAccount())

      challengingAccs.size shouldBe accNum

      val transfers = challengingAccs.zipWithIndex.map { case (acc, idx) =>
        TxHelpers.transfer(TxHelpers.defaultSigner, acc.toAddress, (1000 + idx).waves)
      } :+ TxHelpers.transfer(TxHelpers.defaultSigner, challengedMiner.toAddress, 1000.waves)

      d.appendBlock(transfers*)
      (1 to 999).foreach(_ => d.appendBlock())

      val allChallengingAccounts = challenger.getChallengingAccounts(challengedMiner.toAddress)

      challenger.pickBestAccount(allChallengingAccounts.explicitGet()) shouldBe Right(allChallengingAccounts.explicitGet().minBy(_._2))
    }
  }

  property("NODE-888. ChallengedHeader should contain info from original block header") {
    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get
      d.appendBlock(TxHelpers.transfer(TxHelpers.defaultSigner, challengingMiner.toAddress, 1000.waves))
      (1 to 999).foreach(_ => d.appendBlock())
      val originalBlock = d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, stateHash = Some(Some(invalidStateHash)))
      appendAndCheck(originalBlock, d) { block =>
        block.header.challengedHeader shouldBe defined
        val challengedHeader = block.header.challengedHeader.get

        challengedHeader.timestamp shouldBe originalBlock.header.timestamp
        challengedHeader.baseTarget shouldBe originalBlock.header.baseTarget
        challengedHeader.generationSignature shouldBe originalBlock.header.generationSignature
        challengedHeader.featureVotes shouldBe originalBlock.header.featureVotes
        challengedHeader.generator shouldBe originalBlock.header.generator
        challengedHeader.rewardVote shouldBe originalBlock.header.rewardVote
        challengedHeader.stateHash shouldBe originalBlock.header.stateHash
        challengedHeader.headerSignature shouldBe originalBlock.signature
      }
    }
  }

  property(
    s"NODE-889. Block without challenge (before ${BlockchainFeatures.TransactionStateSnapshot} activation) should not contain ChallengedHeader"
  ) {
    val sender           = TxHelpers.signer(1)
    val challengedMiner  = TxHelpers.signer(2)
    val challengingMiner = TxHelpers.signer(3)
    withDomain(DomainPresets.BlockRewardDistribution, balances = AddrWithBalance.enoughBalances(sender, challengedMiner, challengingMiner)) { d =>
      d.appendBlock()
      val txs = Seq(TxHelpers.transfer(sender, amount = 1), TxHelpers.transfer(sender, amount = 2))
      val challengedBlock =
        d.createBlock(Block.ProtoBlockVersion, txs, strictTime = true, generator = challengedMiner)
      val blockWithChallenge =
        d.createChallengingBlock(challengingMiner, challengedBlock, strictTime = true)

      testTime.setTime(blockWithChallenge.header.timestamp.max(challengedBlock.header.timestamp))
      createBlockAppender(d)(blockWithChallenge).runSyncUnsafe() shouldBe Left(
        BlockAppendError("Challenged header is not supported yet", blockWithChallenge)
      )
    }
  }

  property("NODE-890. Challenging block should contain all transactions from original block") {
    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get
      d.appendBlock(TxHelpers.transfer(TxHelpers.defaultSigner, challengingMiner.toAddress, 1000.waves))
      (1 to 999).foreach(_ => d.appendBlock())
      val originalBlock = d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, stateHash = Some(Some(invalidStateHash)))
      appendAndCheck(originalBlock, d) { block =>
        block.transactionData shouldBe originalBlock.transactionData
      }
    }
  }

  property("NODE-891. Challenging block should contain only transactions from original block") {
    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get
      d.appendBlock(TxHelpers.transfer(TxHelpers.defaultSigner, challengingMiner.toAddress, 1000.waves))
      (1 to 999).foreach(_ => d.appendBlock())
      val originalBlock = d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, stateHash = Some(Some(invalidStateHash)))
      val invalidChallengingBlock = d.createChallengingBlock(
        challengingMiner,
        originalBlock,
        stateHash = None,
        txs = Some(originalBlock.transactionData :+ TxHelpers.transfer(TxHelpers.defaultSigner))
      )

      d.appendBlockE(invalidChallengingBlock) shouldBe Left(
        GenericError(s"Invalid block challenge: ${GenericError(s"Block ${invalidChallengingBlock.toOriginal} has invalid signature")}")
      )

      val correctChallengingBlock = d.createChallengingBlock(challengingMiner, originalBlock, stateHash = None)
      d.appendBlockE(correctChallengingBlock) should beRight

      val microblock = d.createMicroBlock(None, Some(challengingMiner))(TxHelpers.transfer(TxHelpers.defaultSigner))
      d.appendMicroBlockE(microblock) shouldBe Left(MicroBlockAppendError("Base block has challenged header", microblock))
    }
  }

  property("NODE-892. Challenging block should reference the same block as original") {
    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get
      d.appendBlock(TxHelpers.transfer(TxHelpers.defaultSigner, challengingMiner.toAddress, 1000.waves))
      (1 to 999).foreach(_ => d.appendBlock())
      val originalBlock = d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, stateHash = Some(Some(invalidStateHash)))
      appendAndCheck(originalBlock, d) { block =>
        block.header.reference shouldBe originalBlock.header.reference
      }
    }
  }

  property("NODE-893. Challenging block can't reference blocks before previous") {
    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get
      d.appendBlock(TxHelpers.transfer(TxHelpers.defaultSigner, challengingMiner.toAddress, 1000.waves))
      (1 to 999).foreach(_ => d.appendBlock())
      val grandParent = d.blockchain.blockHeader(d.blockchain.height - 2).map(_.id())
      val originalBlock =
        d.createBlock(Block.ProtoBlockVersion, Seq.empty, ref = grandParent, strictTime = true, stateHash = Some(Some(invalidStateHash)))
      val challengingBlock = d.createChallengingBlock(challengingMiner, originalBlock, stateHash = None, ref = grandParent)

      d.appendBlockE(challengingBlock) shouldBe Left(BlockAppendError("References incorrect or non-existing block", challengingBlock))
    }
  }

  property("NODE-894. Node should stop accepting of subsequent microblocks after receiving microblock with invalid state hash") {
    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
      d.appendBlock()
      val lastBlockIdBefore = d.lastBlockId
      d.appendBlock()

      val txs = () => Seq(TxHelpers.transfer(amount = 1.waves), TxHelpers.transfer(amount = 2.waves))

      val appender = createMicroBlockAppender(d)
      val channel  = new EmbeddedChannel()

      val lastValidMicroblock = d.createMicroBlock()(txs()*)
      appender(channel, lastValidMicroblock).runSyncUnsafe()

      val lastValidBlockId  = d.lastBlockId
      val invalidMicroblock = d.createMicroBlock(Some(invalidStateHash))(txs()*)
      val invalidBlockId = Block
        .create(
          d.lastBlock,
          d.lastBlock.transactionData ++ invalidMicroblock.transactionData,
          invalidMicroblock.totalResBlockSig,
          invalidMicroblock.stateHash
        )
        .id()

      channel.isOpen shouldBe true

      appender(channel, invalidMicroblock).runSyncUnsafe()

      channel.isOpen shouldBe false
      d.lastBlockId shouldBe lastBlockIdBefore

      val lastBlockId = d.lastBlockId
      appender(null, d.createMicroBlock(ref = Some(invalidBlockId))(txs()*)).runSyncUnsafe()
      d.lastBlockId shouldBe lastBlockId
      appender(null, d.createMicroBlock(ref = Some(lastValidBlockId))(txs()*)).runSyncUnsafe()
      d.lastBlockId shouldBe lastBlockId
    }
  }

  property("NODE-895. Challenged miner should have correct balances") {
    val challengedMiner = TxHelpers.signer(1)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get
      d.appendBlock(
        TxHelpers.transfer(TxHelpers.defaultSigner, challengingMiner.toAddress, 1000.waves),
        TxHelpers.transfer(TxHelpers.defaultSigner, challengedMiner.toAddress, 2000.waves)
      )
      (1 to 999).foreach(_ => d.appendBlock())
      val originalBlock =
        d.createBlock(
          Block.ProtoBlockVersion,
          Seq(TxHelpers.transfer(challengedMiner, TxHelpers.defaultAddress, amount = 1.waves)),
          strictTime = true,
          generator = challengedMiner,
          stateHash = Some(Some(invalidStateHash))
        )
      val challengingBlock = d.createChallengingBlock(challengingMiner, originalBlock)

      val effBalanceBefore = d.blockchain.effectiveBalance(challengedMiner.toAddress, 0)

      d.appendBlockE(challengingBlock) should beRight
      d.blockchain.effectiveBalance(challengedMiner.toAddress, 0) shouldBe 0L

      d.appendBlock()
      d.blockchain.effectiveBalance(challengedMiner.toAddress, 0) shouldBe effBalanceBefore - 1.waves - TestValues.fee
    }
  }

  property("NODE-898. Block reward and fees should be distributed to challenging miner") {
    val sender = TxHelpers.signer(1)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves))

      (1 to 999).foreach(_ => d.appendBlock())
      val prevBlockTx = TxHelpers.transfer(sender)

      d.appendBlock(prevBlockTx)

      val challengedBlockTxs = Seq(TxHelpers.transfer(sender), TxHelpers.transfer(sender))
      val originalBlock      = d.createBlock(Block.ProtoBlockVersion, challengedBlockTxs, strictTime = true, stateHash = Some(Some(invalidStateHash)))
      val originalMinerBalance    = d.balance(originalBlock.header.generator.toAddress)
      val challengingMinerBalance = d.balance(challengingMiner.toAddress)
      val challengingBlock        = d.createChallengingBlock(challengingMiner, originalBlock)

      d.appendBlockE(challengingBlock) should beRight

      d.balance(originalBlock.header.generator.toAddress) shouldBe originalMinerBalance
      d.balance(
        challengingMiner.toAddress
      ) shouldBe challengingMinerBalance + getLastBlockMinerReward(d) +
        (prevBlockTx.fee.value - BlockDiffer.CurrentBlockFeePart(prevBlockTx.fee.value)) +
        challengedBlockTxs.map(tx => BlockDiffer.CurrentBlockFeePart(tx.fee.value)).sum
    }
  }

  property("NODE-899. Transactions that become invalid in challenging block should have elided status") {
    val sender          = TxHelpers.signer(1)
    val challengedMiner = TxHelpers.signer(2)
    val recipient       = TxHelpers.signer(3)
    val recipientEth    = TxHelpers.signer(4).toEthKeyPair
    val dApp            = TxHelpers.signer(5)
    withDomain(
      DomainPresets.TransactionStateSnapshot.configure(_.copy(minAssetInfoUpdateInterval = 0)),
      balances = AddrWithBalance.enoughBalances(sender, dApp)
    ) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      val script = TestCompiler(V6).compileContract(
        """
          |@Callable(i)
          |func foo() = []
          |""".stripMargin
      )
      val assetScript = TestCompiler(V6).compileAsset("true")
      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves),
        TxHelpers.transfer(sender, challengedMiner.toAddress, 1000.waves),
        TxHelpers.setScript(dApp, script)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val issue             = TxHelpers.issue(recipient)
      val issueSmart        = TxHelpers.issue(recipient, name = "smart", script = Some(assetScript))
      val lease             = TxHelpers.lease(recipient)
      val challengedBlockTx = TxHelpers.transfer(challengedMiner, recipient.toAddress, 1001.waves)
      val recipientTxs = Seq(
        issue,
        issueSmart,
        TxHelpers.burn(issue.asset, sender = recipient),
        TxHelpers.createAlias("alias", recipient),
        TxHelpers.dataSingle(recipient),
        TxHelpers.exchange(
          TxHelpers.order(OrderType.BUY, issue.asset, Waves, matcher = recipient),
          TxHelpers.order(OrderType.SELL, issue.asset, Waves, matcher = recipient),
          recipient
        ),
        TxHelpers.invoke(dApp.toAddress, Some("foo"), invoker = recipient),
        lease,
        TxHelpers.leaseCancel(lease.id(), recipient),
        TxHelpers
          .massTransfer(recipient, Seq(ParsedTransfer(recipientEth.toWavesAddress, TxNonNegativeAmount.unsafeFrom(1.waves))), fee = TestValues.fee),
        TxHelpers.reissue(issue.asset, recipient),
        TxHelpers.setAssetScript(recipient, issueSmart.asset, assetScript, fee = 2.waves),
        TxHelpers.transfer(recipient, recipientEth.toWavesAddress, 100.waves),
        TxHelpers.sponsor(issue.asset, sender = recipient),
        TxHelpers.updateAssetInfo(issue.assetId, sender = recipient),
        TxHelpers.setScript(recipient, script),
        EthTxGenerator.generateEthTransfer(recipientEth, dApp.toAddress, 1, Waves),
        EthTxGenerator.generateEthInvoke(recipientEth, dApp.toAddress, "foo", Seq.empty, Seq.empty)
      )
      val validOriginalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        challengedBlockTx +: recipientTxs,
        strictTime = true,
        generator = challengedMiner
      )
      val invalidOriginalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        challengedBlockTx +: recipientTxs,
        strictTime = true,
        generator = challengedMiner,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = d.createChallengingBlock(challengingMiner, invalidOriginalBlock)

      d.appendBlockE(validOriginalBlock) should beRight
      recipientTxs.foreach { tx =>
        d.transactionsApi.transactionById(tx.id()).map(_.status).contains(TxMeta.Status.Succeeded) shouldBe true
      }

      d.rollbackTo(validOriginalBlock.header.reference)

      d.appendBlockE(challengingBlock) should beRight

      val blockRewards = getLastBlockRewards(d)
      // block snapshot contains only txs and block reward
      val blockSnapshot = d.blockchain.bestLiquidSnapshot.get
      val expectedSnapshot = StateSnapshot
        .build(
          d.rocksDBWriter,
          Map(challengingMiner.toAddress                                                        -> Portfolio.waves(blockRewards.miner)) ++
            d.blockchain.settings.functionalitySettings.daoAddressParsed.toOption.flatten.map(_ -> Portfolio.waves(blockRewards.daoAddress)) ++
            d.blockchain.settings.functionalitySettings.xtnBuybackAddressParsed.toOption.flatten
              .map(_ -> Portfolio.waves(blockRewards.xtnBuybackAddress)),
          transactions = blockSnapshot.transactions
        )
        .explicitGet()

      blockSnapshot shouldBe expectedSnapshot
      blockSnapshot.transactions.foreach(_._2.status shouldBe TxMeta.Status.Elided)
      recipientTxs.foreach { tx =>
        d.transactionsApi.transactionById(tx.id()).map(_.status).contains(TxMeta.Status.Elided) shouldBe true
      }
    }
  }

  property("NODE-900. Transactions can change status in challenging block") {
    val sender          = TxHelpers.signer(1)
    val challengedMiner = TxHelpers.signer(2)
    val invoker         = TxHelpers.signer(3)
    val invokerEth      = TxHelpers.signer(4).toEthKeyPair
    val dApp            = TxHelpers.signer(5)
    val buyer           = TxHelpers.signer(6)
    val matcher         = TxHelpers.signer(7)
    withDomain(
      DomainPresets.TransactionStateSnapshot,
      balances = AddrWithBalance.enoughBalances(sender, dApp, invoker, buyer, matcher)
    ) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      val script = TestCompiler(V6).compileContract(
        s"""
           |@Callable(i)
           |func foo() = {
           |  strict a = ${(1 to 10).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}
           |  if (lastBlock.generator == Address(base58'${challengedMiner.toAddress}')) then {
           |    [StringEntry(toBase58String(i.transactionId), "value")]
           |  } else {
           |    throw("fail")
           |  }
           |}
           |""".stripMargin
      )
      val assetVerifier = TestCompiler(V6).compileAsset(
        s"""
           |strict a = ${(1 to 10).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}
           |if (lastBlock.generator == Address(base58'${challengedMiner.toAddress}')) then {
           |    true
           |  } else {
           |    false
           |  }
           |""".stripMargin
      )

      val issue = TxHelpers.issue(sender, script = Some(assetVerifier))

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves),
        TxHelpers.transfer(sender, challengedMiner.toAddress, 1000.waves),
        TxHelpers.transfer(sender, invokerEth.toWavesAddress, 1000.waves),
        TxHelpers.setScript(dApp, script),
        issue
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val invokeTxs = Seq(
        TxHelpers.invoke(dApp.toAddress, Some("foo"), invoker = invoker),
        EthTxGenerator.generateEthInvoke(invokerEth, dApp.toAddress, "foo", Seq.empty, Seq.empty)
      )
      val price = 2.waves
      val exchangeTx = TxHelpers.exchange(
        TxHelpers.order(OrderType.BUY, issue.asset, Waves, price = price, sender = buyer, matcher = matcher),
        TxHelpers.order(OrderType.SELL, issue.asset, Waves, price = price, sender = sender, matcher = matcher),
        matcher,
        price = (price / 1e8).toLong,
        version = TxVersion.V3
      )
      val txs = exchangeTx +: invokeTxs
      val validOriginalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        txs,
        strictTime = true,
        generator = challengedMiner
      )
      val invalidOriginalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        txs,
        strictTime = true,
        generator = challengedMiner,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = d.createChallengingBlock(challengingMiner, invalidOriginalBlock)

      val sellerAssetBalance = issue.quantity.value
      val sellerWavesBalance = d.balance(sender.toAddress)
      val buyerAssetBalance  = 0
      val buyerWavesBalance  = d.balance(buyer.toAddress)

      d.appendBlockE(validOriginalBlock) should beRight
      invokeTxs.foreach { tx =>
        d.transactionsApi.transactionById(tx.id()).map(_.status).contains(TxMeta.Status.Succeeded) shouldBe true
        d.blockchain.accountData(dApp.toAddress, tx.id().toString) shouldBe Some(StringDataEntry(tx.id().toString, "value"))
      }
      d.transactionsApi.transactionById(exchangeTx.id()).map(_.status).contains(TxMeta.Status.Succeeded) shouldBe true
      d.balance(sender.toAddress, issue.asset) shouldBe sellerAssetBalance - exchangeTx.order1.amount.value
      d.balance(sender.toAddress) shouldBe sellerWavesBalance + exchangeTx.price.value - exchangeTx.order2.matcherFee.value
      d.balance(buyer.toAddress, issue.asset) shouldBe buyerAssetBalance + exchangeTx.order1.amount.value
      d.balance(buyer.toAddress) shouldBe buyerWavesBalance - exchangeTx.price.value - exchangeTx.order1.matcherFee.value

      d.rollbackTo(validOriginalBlock.header.reference)

      d.appendBlockE(challengingBlock) should beRight

      invokeTxs.foreach { tx =>
        d.transactionsApi.transactionById(tx.id()).map(_.status).contains(TxMeta.Status.Failed) shouldBe true
        d.blockchain.accountData(dApp.toAddress, tx.id().toString) shouldBe None
      }
      d.transactionsApi.transactionById(exchangeTx.id()).map(_.status).contains(TxMeta.Status.Failed) shouldBe true
      d.balance(sender.toAddress, issue.asset) shouldBe sellerAssetBalance
      d.balance(sender.toAddress) shouldBe sellerWavesBalance
      d.balance(buyer.toAddress, issue.asset) shouldBe buyerAssetBalance
      d.balance(buyer.toAddress) shouldBe buyerWavesBalance
    }
  }

  property("NODE-901. Elided transaction sender should not pay fee") {
    val sender          = TxHelpers.signer(1)
    val challengedMiner = TxHelpers.signer(2)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves),
        TxHelpers.transfer(sender, challengedMiner.toAddress, 1000.waves)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val challengedBlockTx = TxHelpers.transfer(challengedMiner, amount = 1001.waves)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq(challengedBlockTx),
        strictTime = true,
        generator = challengedMiner,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = d.createChallengingBlock(challengingMiner, originalBlock)

      val elidedTxSenderBalance = d.balance(challengedMiner.toAddress)

      d.appendBlockE(challengingBlock) should beRight

      d.transactionsApi.transactionById(challengedBlockTx.id()).map(_.status).contains(TxMeta.Status.Elided) shouldBe true
      d.balance(challengedMiner.toAddress) shouldBe elidedTxSenderBalance
    }
  }

  property("NODE-902. Elided transaction should have unique ID") {
    val sender          = TxHelpers.signer(1)
    val challengedMiner = TxHelpers.signer(2)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 10000.waves),
        TxHelpers.transfer(sender, challengedMiner.toAddress, 10000.waves)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val challengedBlockTx = TxHelpers.transfer(challengedMiner, amount = 10001.waves)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq(challengedBlockTx),
        strictTime = true,
        generator = challengedMiner,
        stateHash = Some(Some(invalidStateHash))
      )

      appendAndCheck(originalBlock, d) { block =>
        d.transactionsApi.transactionById(challengedBlockTx.id()).map(_.status).contains(TxMeta.Status.Elided) shouldBe true
        block.transactionData.head.id() shouldBe challengedBlockTx.id()

        d.appendBlock(TxHelpers.transfer(sender, challengedMiner.toAddress, 10.waves))
        d.transactionDiffer(challengedBlockTx).resultE should produce("AlreadyInTheState")
      }
    }
  }

  property("NODE-904. /transactions/merkleProof should return proofs for elided transactions") {
    val sender          = TxHelpers.signer(1)
    val challengedMiner = TxHelpers.signer(2)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves),
        TxHelpers.transfer(sender, challengedMiner.toAddress, 1000.waves)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val challengedBlockTx = TxHelpers.transfer(challengedMiner, amount = 1001.waves)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq(challengedBlockTx),
        strictTime = true,
        generator = challengedMiner,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = d.createChallengingBlock(challengingMiner, originalBlock)

      d.appendBlockE(challengingBlock) should beRight
      d.transactionsApi.transactionById(challengedBlockTx.id()).map(_.status).contains(TxMeta.Status.Elided) shouldBe true

      val route = new TransactionsApiRoute(
        d.settings.restAPISettings,
        d.commonApi.transactions,
        d.wallet,
        d.blockchain,
        () => d.blockchain,
        () => 0,
        (t, _) => d.commonApi.transactions.broadcastTransaction(t),
        testTime,
        new RouteTimeout(60.seconds)(sharedScheduler)
      ).route

      d.liquidAndSolidAssert { () =>
        Get(s"/transactions/merkleProof?id=${challengedBlockTx.id()}") ~> route ~> check {
          val proof = responseAs[JsArray].value.head.as[JsObject]
          (proof \ "id").as[String] shouldBe challengedBlockTx.id().toString
          (proof \ "transactionIndex").as[Int] shouldBe 0
          (proof \ "merkleProof").as[Seq[String]].head shouldBe ByteStr(
            Merkle
              .mkProofs(
                0,
                Merkle.mkLevels(Seq(challengedBlockTx).map(PBTransactions.toByteArrayMerkle))
              )
              .head
          ).toString
        }
      }
    }
  }

  property("NODE-905. transactionHeightById(), transferTransactionById() and transactionById() should return unit for elided transactions") {
    val sender          = TxHelpers.signer(1)
    val challengedMiner = TxHelpers.signer(2)
    val dApp            = TxHelpers.signer(3)

    val script =
      TestCompiler(V6).compileContract(
        """
          |@Callable(i)
          |func foo(id: ByteVector) = {
          |   let txHeight = transactionHeightById(id)
          |   let transfer = transferTransactionById(id)
          |
          |   [BooleanEntry("check", txHeight == unit && transfer == unit)]
          |}
          |
          |""".stripMargin
      )
    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender, dApp)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves),
        TxHelpers.transfer(sender, challengedMiner.toAddress, 1000.waves),
        TxHelpers.setScript(dApp, script)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val challengedBlockTx = TxHelpers.transfer(challengedMiner, amount = 1001.waves)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq(challengedBlockTx),
        strictTime = true,
        generator = challengedMiner,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = d.createChallengingBlock(challengingMiner, originalBlock)

      d.appendBlockE(challengingBlock) should beRight
      d.transactionsApi.transactionById(challengedBlockTx.id()).map(_.status).contains(TxMeta.Status.Elided) shouldBe true

      d.appendBlock(TxHelpers.invoke(dApp.toAddress, Some("foo"), Seq(CONST_BYTESTR(challengedBlockTx.id()).explicitGet()), invoker = sender))
      d.blockchain.accountData(dApp.toAddress, "check") shouldBe Some(BooleanDataEntry("check", value = true))
    }
  }

  property("NODE-906. wavesBalance() should return correct balances for challenged and challenging addresses") {
    val sender          = TxHelpers.signer(1)
    val challengedMiner = TxHelpers.signer(2)
    val dApp            = TxHelpers.signer(3)

    val challengedRegularKey     = "challengedRegular"
    val challengedEffectiveKey   = "challengedEffective"
    val challengedGeneratingKey  = "challengedGenerating"
    val challengingRegularKey    = "challengingRegular"
    val challengingEffectiveKey  = "challengingEffective"
    val challengingGeneratingKey = "challengingGenerating"

    val script =
      TestCompiler(V6).compileContract(
        s"""
           |@Callable(i)
           |func foo(challenged: ByteVector, challenging: ByteVector) = {
           |   let challengedBalance = wavesBalance(Address(challenged))
           |   let challengingBalance = wavesBalance(Address(challenging))
           |
           |   [
           |     IntegerEntry("$challengedRegularKey", challengedBalance.regular),
           |     IntegerEntry("$challengedEffectiveKey", challengedBalance.effective),
           |     IntegerEntry("$challengedGeneratingKey", challengedBalance.generating),
           |     IntegerEntry("$challengingRegularKey", challengingBalance.regular),
           |     IntegerEntry("$challengingEffectiveKey", challengingBalance.effective),
           |     IntegerEntry("$challengingGeneratingKey", challengingBalance.generating)
           |   ]
           |}
           |
           |""".stripMargin
      )
    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender, dApp)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      val initChallengedBalance  = 2000.waves
      val initChallengingBalance = 1000.waves

      val invoke = () =>
        TxHelpers.invoke(
          dApp.toAddress,
          Some("foo"),
          Seq(
            CONST_BYTESTR(ByteStr(challengedMiner.toAddress.bytes)).explicitGet(),
            CONST_BYTESTR(ByteStr(challengingMiner.toAddress.bytes)).explicitGet()
          ),
          invoker = sender
        )

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, initChallengingBalance),
        TxHelpers.transfer(sender, challengedMiner.toAddress, initChallengedBalance),
        TxHelpers.setScript(dApp, script)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      d.appendBlockE(invoke()) should beRight

      d.blockchain.accountData(dApp.toAddress, challengedRegularKey) shouldBe Some(IntegerDataEntry(challengedRegularKey, initChallengedBalance))
      d.blockchain.accountData(dApp.toAddress, challengedEffectiveKey) shouldBe Some(IntegerDataEntry(challengedEffectiveKey, initChallengedBalance))
      d.blockchain.accountData(dApp.toAddress, challengedGeneratingKey) shouldBe Some(
        IntegerDataEntry(challengedGeneratingKey, initChallengedBalance)
      )
      d.blockchain.accountData(dApp.toAddress, challengingRegularKey) shouldBe Some(IntegerDataEntry(challengingRegularKey, initChallengingBalance))
      d.blockchain.accountData(dApp.toAddress, challengingEffectiveKey) shouldBe Some(
        IntegerDataEntry(challengingEffectiveKey, initChallengingBalance)
      )
      d.blockchain.accountData(dApp.toAddress, challengingGeneratingKey) shouldBe Some(
        IntegerDataEntry(challengingGeneratingKey, initChallengingBalance)
      )

      val challengedBlockTx = invoke()
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq(challengedBlockTx),
        strictTime = true,
        generator = challengedMiner,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = d.createChallengingBlock(challengingMiner, originalBlock)

      d.appendBlockE(challengingBlock) should beRight

      d.blockchain.accountData(dApp.toAddress, challengedRegularKey) shouldBe Some(IntegerDataEntry(challengedRegularKey, initChallengedBalance))
      d.blockchain.accountData(dApp.toAddress, challengedEffectiveKey) shouldBe Some(IntegerDataEntry(challengedEffectiveKey, 0))
      d.blockchain.accountData(dApp.toAddress, challengedGeneratingKey) shouldBe Some(
        IntegerDataEntry(challengedGeneratingKey, 0)
      )
      d.blockchain.accountData(dApp.toAddress, challengingRegularKey) shouldBe Some(
        IntegerDataEntry(
          challengingRegularKey,
          initChallengingBalance + getLastBlockMinerReward(d) + (challengedBlockTx.fee.value - CurrentBlockFeePart(
            challengedBlockTx.fee.value
          ))
        )
      )
      d.blockchain.accountData(dApp.toAddress, challengingEffectiveKey) shouldBe Some(
        IntegerDataEntry(
          challengingEffectiveKey,
          initChallengingBalance + getLastBlockMinerReward(d) + (challengedBlockTx.fee.value - CurrentBlockFeePart(
            challengedBlockTx.fee.value
          ))
        )
      )
      d.blockchain.accountData(dApp.toAddress, challengingGeneratingKey) shouldBe Some(
        IntegerDataEntry(challengingGeneratingKey, initChallengingBalance)
      )

      d.appendBlock(invoke())

      d.blockchain.accountData(dApp.toAddress, challengedRegularKey) shouldBe Some(IntegerDataEntry(challengedRegularKey, initChallengedBalance))
      d.blockchain.accountData(dApp.toAddress, challengedEffectiveKey) shouldBe Some(IntegerDataEntry(challengedEffectiveKey, initChallengedBalance))
      d.blockchain.accountData(dApp.toAddress, challengedGeneratingKey) shouldBe Some(
        IntegerDataEntry(challengedGeneratingKey, 0)
      )
      d.blockchain.accountData(dApp.toAddress, challengingRegularKey) shouldBe Some(
        IntegerDataEntry(
          challengingRegularKey,
          initChallengingBalance + getLastBlockMinerReward(d) + (challengedBlockTx.fee.value - CurrentBlockFeePart(
            challengedBlockTx.fee.value
          )) + CurrentBlockFeePart(challengedBlockTx.fee.value)
        )
      )
      d.blockchain.accountData(dApp.toAddress, challengingEffectiveKey) shouldBe Some(
        IntegerDataEntry(
          challengingEffectiveKey,
          initChallengingBalance + getLastBlockMinerReward(d) + (challengedBlockTx.fee.value - CurrentBlockFeePart(
            challengedBlockTx.fee.value
          )) + CurrentBlockFeePart(challengedBlockTx.fee.value)
        )
      )
      d.blockchain.accountData(dApp.toAddress, challengingGeneratingKey) shouldBe Some(
        IntegerDataEntry(challengingGeneratingKey, initChallengingBalance)
      )
    }
  }

  property("NODE-907. blockInfoByHeight() should return correct data for challenging block") {
    val sender = TxHelpers.signer(1)
    val dApp   = TxHelpers.signer(2)

    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender, dApp)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq.empty,
        strictTime = true,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = d.createChallengingBlock(challengingMiner, originalBlock)

      d.appendBlockE(challengingBlock) should beRight
      val vrf = d.blockchain.vrf(1002).get

      val script =
        TestCompiler(V6).compileContract(
          s"""
             |@Callable(i)
             |func foo(h: Int) = {
             |   let blockInfo = value(blockInfoByHeight(h))
             |
             |   [BooleanEntry("check", blockInfo.timestamp == ${challengingBlock.header.timestamp} &&
             |   blockInfo.baseTarget == ${challengingBlock.header.baseTarget} &&
             |   blockInfo.generationSignature == base58'${challengingBlock.header.generationSignature}' &&
             |   blockInfo.height == 1002 &&
             |   blockInfo.generator == Address(base58'${challengingBlock.header.generator.toAddress}') &&
             |   blockInfo.generatorPublicKey == base58'${challengingBlock.header.generator}' &&
             |   blockInfo.vrf == base58'$vrf')]
             |}
             |
             |""".stripMargin
        )

      d.appendBlock(TxHelpers.setScript(dApp, script))
      d.appendBlock(TxHelpers.invoke(dApp.toAddress, Some("foo"), Seq(CONST_LONG(1002)), invoker = sender))

      d.blockchain.accountData(dApp.toAddress, "check") shouldBe Some(BooleanDataEntry("check", value = true))
    }
  }

  property("NODE-909. Empty key block can be challenged") {
    val sender = TxHelpers.signer(1)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender, defaultSigner)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq.empty,
        strictTime = true,
        stateHash = Some(Some(invalidStateHash))
      )

      appendAndCheck(originalBlock, d) { block =>
        block.header.challengedHeader shouldBe defined
        val challengedHeader = block.header.challengedHeader.get

        challengedHeader.timestamp shouldBe originalBlock.header.timestamp
        challengedHeader.baseTarget shouldBe originalBlock.header.baseTarget
        challengedHeader.generationSignature shouldBe originalBlock.header.generationSignature
        challengedHeader.featureVotes shouldBe originalBlock.header.featureVotes
        challengedHeader.generator shouldBe originalBlock.header.generator
        challengedHeader.rewardVote shouldBe originalBlock.header.rewardVote
        challengedHeader.stateHash shouldBe originalBlock.header.stateHash
        challengedHeader.headerSignature shouldBe originalBlock.signature
      }
    }
  }

  property(s"NODE-910. Block at ${BlockchainFeatures.TransactionStateSnapshot} activation height can be challenged") {
    withDomain(
      DomainPresets.BlockRewardDistribution
        .addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
        .setFeaturesHeight(BlockchainFeatures.TransactionStateSnapshot -> 1003),
      balances = AddrWithBalance.enoughBalances(defaultSigner)
    ) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(TxHelpers.transfer(defaultSigner, challengingMiner.toAddress, 1000.waves))
      (1 to 999).foreach(_ => d.appendBlock())
      d.appendBlock(
        d.createBlock(
          Block.ProtoBlockVersion,
          Seq.empty,
          strictTime = true
        )
      )

      d.blockchain.isFeatureActivated(BlockchainFeatures.TransactionStateSnapshot) shouldBe false

      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq.empty,
        strictTime = true,
        stateHash = Some(Some(invalidStateHash))
      )

      appendAndCheck(originalBlock, d) { block =>
        block.header.challengedHeader shouldBe defined
        val challengedHeader = block.header.challengedHeader.get

        challengedHeader.timestamp shouldBe originalBlock.header.timestamp
        challengedHeader.baseTarget shouldBe originalBlock.header.baseTarget
        challengedHeader.generationSignature shouldBe originalBlock.header.generationSignature
        challengedHeader.featureVotes shouldBe originalBlock.header.featureVotes
        challengedHeader.generator shouldBe originalBlock.header.generator
        challengedHeader.rewardVote shouldBe originalBlock.header.rewardVote
        challengedHeader.stateHash shouldBe originalBlock.header.stateHash
        challengedHeader.headerSignature shouldBe originalBlock.signature
      }
    }
  }

  property(s"NODE-911. Rollback should work correctly with challenging blocks") {
    val sender          = TxHelpers.signer(1)
    val challengedMiner = TxHelpers.signer(2)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 10000.waves),
        TxHelpers.transfer(sender, challengedMiner.toAddress, 10000.waves)
      )
      (1 to 999).foreach(_ => d.appendBlock())
      val rollbackTarget = d.blockchain.lastBlockId.get

      val txs = Seq(TxHelpers.transfer(challengedMiner, amount = 10001.waves))
      rollbackMiddleScenario(d, challengedMiner, txs)
      val middleScenarioStateHash = d.lastBlock.header.stateHash
      middleScenarioStateHash shouldBe defined
      d.rollbackTo(rollbackTarget)
      rollbackMiddleScenario(d, challengedMiner, txs)

      d.lastBlock.header.stateHash shouldBe middleScenarioStateHash

      d.rollbackTo(rollbackTarget)

      rollbackLastScenario(d, challengedMiner, txs)
      val lastScenarioStateHash = d.lastBlock.header.stateHash
      lastScenarioStateHash shouldBe defined
      d.rollbackTo(rollbackTarget)

      rollbackLastScenario(d, challengedMiner, txs)

      d.lastBlock.header.stateHash shouldBe lastScenarioStateHash
    }

    withDomain(
      DomainPresets.BlockRewardDistribution
        .addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
        .setFeaturesHeight(BlockchainFeatures.TransactionStateSnapshot -> 1008),
      balances = AddrWithBalance.enoughBalances(sender)
    ) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 10000.waves),
        TxHelpers.transfer(sender, challengedMiner.toAddress, 10000.waves)
      )
      (1 to 999).foreach(_ => d.appendBlock())

      val rollbackTarget = d.blockchain.lastBlockId.get

      val txs = Seq(TxHelpers.transfer(challengedMiner, amount = 10001.waves))
      rollbackActivationHeightScenario(d, challengedMiner, txs)
      val stateHash = d.lastBlock.header.stateHash
      stateHash shouldBe defined
      d.rollbackTo(rollbackTarget)
      rollbackActivationHeightScenario(d, challengedMiner, txs)

      d.lastBlock.header.stateHash shouldBe stateHash
    }
  }

  property("NODE-912. ExtensionAppender should append challenging block correctly") {
    withDomain(settings, balances = AddrWithBalance.enoughBalances(defaultSigner)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(
        TxHelpers.transfer(defaultSigner, challengingMiner.toAddress, 1000.waves)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq.empty,
        strictTime = true,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = d.createChallengingBlock(challengingMiner, originalBlock, strictTime = true)

      val extensionAppender =
        ExtensionAppender(d.blockchain, d.utxPool, d.posSelector, testTime, InvalidBlockStorage.NoOp, PeerDatabase.NoOp, appenderScheduler)(null, _)

      testTime.setTime(challengingBlock.header.timestamp)
      extensionAppender(ExtensionBlocks(d.blockchain.score + challengingBlock.blockScore(), Seq(challengingBlock), Map.empty))
        .runSyncUnsafe()
        .explicitGet()

      d.blockchain.height shouldBe 1002

      d.lastBlock.header.challengedHeader shouldBe defined
      val challengedHeader = d.lastBlock.header.challengedHeader.get

      challengedHeader.timestamp shouldBe originalBlock.header.timestamp
      challengedHeader.baseTarget shouldBe originalBlock.header.baseTarget
      challengedHeader.generationSignature shouldBe originalBlock.header.generationSignature
      challengedHeader.featureVotes shouldBe originalBlock.header.featureVotes
      challengedHeader.generator shouldBe originalBlock.header.generator
      challengedHeader.rewardVote shouldBe originalBlock.header.rewardVote
      challengedHeader.stateHash shouldBe originalBlock.header.stateHash
      challengedHeader.headerSignature shouldBe originalBlock.signature
    }
  }

  property("NODE-913. Challenging of block with correct state hash is impossible") {
    val sender = TxHelpers.signer(1)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender, defaultSigner)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq(TxHelpers.transfer(sender, challengingMiner.toAddress, 1.waves)),
        strictTime = true
      )

      val challengingBlock = d.createChallengingBlock(challengingMiner, originalBlock, ref = Some(d.lastBlockId))

      d.appendBlockE(challengingBlock) shouldBe Left(GenericError("Invalid block challenge"))
      d.appendBlockE(originalBlock) should beRight
      d.appendBlockE(challengingBlock) shouldBe Left(GenericError("Invalid block challenge"))
    }
  }

  property("NODE-914. Blocks API should return correct data for challenging block") {
    val sender = TxHelpers.signer(1)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender, defaultSigner)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq.empty,
        strictTime = true,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = d.createChallengingBlock(challengingMiner, originalBlock)
      val blockHeight      = 1002

      d.appendBlockE(challengingBlock) should beRight

      val route = new BlocksApiRoute(
        d.settings.restAPISettings,
        d.blocksApi,
        testTime,
        new RouteTimeout(60.seconds)(sharedScheduler)
      ).route

      Get("/blocks/last") ~> route ~> check {
        checkBlockJson(responseAs[JsObject], challengingBlock)
      }
      Get("/blocks/headers/last") ~> route ~> check {
        checkBlockJson(responseAs[JsObject], challengingBlock)
      }

      d.liquidAndSolidAssert { () =>
        Get(s"/blocks/at/$blockHeight") ~> route ~> check {
          checkBlockJson(responseAs[JsObject], challengingBlock)
        }
        Get(s"/blocks/seq/$blockHeight/$blockHeight") ~> route ~> check {
          checkBlockJson(responseAs[JsArray].value.head.as[JsObject], challengingBlock)
        }

        Get(s"/blocks/height/${challengingBlock.id()}") ~> route ~> check {
          (responseAs[JsObject] \ "height").as[Int] shouldBe blockHeight
        }
        Get(s"/blocks/address/${challengingMiner.toAddress}/$blockHeight/$blockHeight") ~> route ~> check {
          checkBlockJson(responseAs[JsArray].value.head.as[JsObject], challengingBlock)
        }
        Get(s"/blocks/headers/at/$blockHeight") ~> route ~> check {
          checkBlockJson(responseAs[JsObject], challengingBlock)
        }
        Get(s"/blocks/headers/seq/$blockHeight/$blockHeight") ~> route ~> check {
          checkBlockJson(responseAs[JsArray].value.head.as[JsObject], challengingBlock)
        }

        Get(s"/blocks/${challengingBlock.id()}") ~> route ~> check {
          checkBlockJson(responseAs[JsObject], challengingBlock)
        }
      }
    }
  }

  property("NODE-915. /transactions/address should return elided transactions") {
    val sender          = TxHelpers.signer(1)
    val challengedMiner = TxHelpers.signer(2)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves),
        TxHelpers.transfer(sender, challengedMiner.toAddress, 1000.waves)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val challengedBlockTx = TxHelpers.transfer(challengedMiner, amount = 1001.waves)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq(challengedBlockTx),
        strictTime = true,
        generator = challengedMiner,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = d.createChallengingBlock(challengingMiner, originalBlock)

      d.appendBlockE(challengingBlock) should beRight
      d.transactionsApi.transactionById(challengedBlockTx.id()).map(_.status).contains(TxMeta.Status.Elided) shouldBe true

      val route = new TransactionsApiRoute(
        d.settings.restAPISettings,
        d.commonApi.transactions,
        d.wallet,
        d.blockchain,
        () => d.blockchain,
        () => 0,
        (t, _) => d.commonApi.transactions.broadcastTransaction(t),
        testTime,
        new RouteTimeout(60.seconds)(sharedScheduler)
      ).route

      d.liquidAndSolidAssert { () =>
        Get(s"/transactions/address/${challengedMiner.toAddress}/limit/10") ~> route ~> check {
          val txResponse = responseAs[JsArray].value.head.as[JsArray].value.head.as[JsObject]
          txResponse shouldBe challengedBlockTx
            .json() ++ Json.obj("height" -> 1002, "spentComplexity" -> 0, "applicationStatus" -> ApplicationStatus.Elided)
        }
      }
    }
  }

  property("NODE-916. /transactions/info should return correct data for elided transactions") {
    val sender          = TxHelpers.signer(1)
    val challengedMiner = TxHelpers.signer(2)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves),
        TxHelpers.transfer(sender, challengedMiner.toAddress, 1000.waves)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val challengedBlockTx = TxHelpers.transfer(challengedMiner, amount = 1001.waves)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq(challengedBlockTx),
        strictTime = true,
        generator = challengedMiner,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = d.createChallengingBlock(challengingMiner, originalBlock)

      d.appendBlockE(challengingBlock) should beRight
      d.transactionsApi.transactionById(challengedBlockTx.id()).map(_.status).contains(TxMeta.Status.Elided) shouldBe true

      val route = new TransactionsApiRoute(
        d.settings.restAPISettings,
        d.commonApi.transactions,
        d.wallet,
        d.blockchain,
        () => d.blockchain,
        () => 0,
        (t, _) => d.commonApi.transactions.broadcastTransaction(t),
        testTime,
        new RouteTimeout(60.seconds)(sharedScheduler)
      ).route

      val extraFields =
        Json.obj("height" -> 1002, "spentComplexity" -> 0, "applicationStatus" -> ApplicationStatus.Elided)
      val expectedResponse = challengedBlockTx.json() ++ extraFields

      d.liquidAndSolidAssert { () =>
        Get(s"/transactions/info/${challengedBlockTx.id()}") ~> route ~> check {
          responseAs[JsObject] shouldBe expectedResponse
        }

        Post("/transactions/info", FormData("id" -> challengedBlockTx.id().toString)) ~> route ~> check {
          responseAs[JsArray].value.head.as[JsObject] shouldBe expectedResponse
        }

        Post(
          "/transactions/info",
          HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> Json.arr(challengedBlockTx.id().toString)).toString())
        ) ~> route ~> check {
          responseAs[JsArray].value.head.as[JsObject] shouldBe expectedResponse
        }
      }
    }
  }

  property("NODE-917. /transactions/status should return correct data for elided transactions") {
    def checkTxStatus(tx: Transaction, confirmations: Int, route: Route): Assertion = {
      val expectedResponse = Json.obj(
        "status"            -> Status.Confirmed,
        "height"            -> 1002,
        "confirmations"     -> confirmations,
        "applicationStatus" -> ApplicationStatus.Elided,
        "spentComplexity"   -> 0,
        "id"                -> tx.id().toString
      )

      Get(s"/transactions/status/${tx.id()}") ~> route ~> check {
        responseAs[JsObject] shouldBe expectedResponse
      }

      Post("/transactions/status", FormData("id" -> tx.id().toString)) ~> route ~> check {
        responseAs[JsArray].value.head.as[JsObject] should matchJson(expectedResponse)
      }

      Post(
        "/transactions/status",
        HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> Json.arr(tx.id().toString)).toString())
      ) ~> route ~> check {
        responseAs[JsArray].value.head.as[JsObject] should matchJson(expectedResponse)
      }
    }

    val sender          = TxHelpers.signer(1)
    val challengedMiner = TxHelpers.signer(2)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves),
        TxHelpers.transfer(sender, challengedMiner.toAddress, 1000.waves)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val challengedBlockTx = TxHelpers.transfer(challengedMiner, amount = 1001.waves)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq(challengedBlockTx),
        strictTime = true,
        generator = challengedMiner,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = d.createChallengingBlock(challengingMiner, originalBlock)

      d.appendBlockE(challengingBlock) should beRight
      d.transactionsApi.transactionById(challengedBlockTx.id()).map(_.status).contains(TxMeta.Status.Elided) shouldBe true

      val route = new TransactionsApiRoute(
        d.settings.restAPISettings,
        d.commonApi.transactions,
        d.wallet,
        d.blockchain,
        () => d.blockchain,
        () => 0,
        (t, _) => d.commonApi.transactions.broadcastTransaction(t),
        testTime,
        new RouteTimeout(60.seconds)(sharedScheduler)
      ).route

      checkTxStatus(challengedBlockTx, 0, route)
      d.appendBlock()
      checkTxStatus(challengedBlockTx, 1, route)
    }
  }

  property("NODE-918. Addresses API should return correct balances for challenged and challenging miners") {
    def checkBalances(
        address: Address,
        expectedRegular: Long,
        expectedEffective: Long,
        expectedGenerating: Long,
        height: Int,
        route: Route
    ): Assertion = {
      Get(s"/addresses/balance/details/$address") ~> route ~> check {
        val balance = responseAs[JsObject]
        (balance \ "regular").as[Long] shouldBe expectedRegular
        (balance \ "generating").as[Long] shouldBe expectedGenerating
        (balance \ "available").as[Long] shouldBe expectedRegular
        (balance \ "effective").as[Long] shouldBe expectedEffective
      }
      Get(s"/addresses/balance/$address") ~> route ~> check {
        val balance = responseAs[JsObject]
        (balance \ "balance").as[Long] shouldBe expectedRegular
      }
      Get(s"/addresses/balance?address=$address&height=$height") ~> route ~> check {
        val balance = responseAs[JsArray].value.head.as[JsObject]
        (balance \ "id").as[String] shouldBe address.toString
        (balance \ "balance").as[Long] shouldBe expectedRegular
      }
      Post(
        "/addresses/balance",
        HttpEntity(ContentTypes.`application/json`, Json.obj("height" -> height, "addresses" -> Seq(address)).toString())
      ) ~> route ~> check {
        val balance = responseAs[JsArray].value.head.as[JsObject]
        (balance \ "id").as[String] shouldBe address.toString
        (balance \ "balance").as[Long] shouldBe expectedRegular
      }
      Get(s"/addresses/effectiveBalance/$address") ~> route ~> check {
        val balance = responseAs[JsObject]
        (balance \ "balance").as[Long] shouldBe expectedEffective
      }
      Get(s"/addresses/effectiveBalance/$address/1000") ~> route ~> check {
        val balance = responseAs[JsObject]
        (balance \ "balance").as[Long] shouldBe expectedGenerating
      }
    }

    val sender          = TxHelpers.signer(1)
    val challengedMiner = TxHelpers.signer(2)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get

      val initChallengingBalance = 1000.waves
      val initChallengedBalance  = 2000.waves

      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, initChallengingBalance),
        TxHelpers.transfer(sender, challengedMiner.toAddress, initChallengedBalance)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq.empty,
        strictTime = true,
        generator = challengedMiner,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = d.createChallengingBlock(challengingMiner, originalBlock)

      val route = new AddressApiRoute(
        d.settings.restAPISettings,
        d.wallet,
        d.blockchain,
        DummyTransactionPublisher.accepting,
        testTime,
        Schedulers.timeBoundedFixedPool(new HashedWheelTimer(), 5.seconds, 1, "rest-time-limited"),
        new RouteTimeout(60.seconds)(sharedScheduler),
        d.accountsApi,
        1000
      ).route

      checkBalances(challengingMiner.toAddress, initChallengingBalance, initChallengingBalance, initChallengingBalance, 1001, route)
      checkBalances(challengedMiner.toAddress, initChallengedBalance, initChallengedBalance, initChallengedBalance, 1001, route)

      d.appendBlockE(challengingBlock) should beRight

      checkBalances(
        challengingMiner.toAddress,
        initChallengingBalance + getLastBlockMinerReward(d),
        initChallengingBalance + getLastBlockMinerReward(d),
        initChallengingBalance,
        1002,
        route
      )
      checkBalances(challengedMiner.toAddress, initChallengedBalance, 0, 0, 1002, route)

      d.appendBlock()

      checkBalances(
        challengingMiner.toAddress,
        initChallengingBalance + getLastBlockMinerReward(d),
        initChallengingBalance + getLastBlockMinerReward(d),
        initChallengingBalance,
        1003,
        route
      )
      checkBalances(challengedMiner.toAddress, initChallengedBalance, initChallengedBalance, 0, 1003, route)

    }
  }

  property("NODE-919. Transactions from challenging block should have unconfirmed status when creating of block is in progress") {
    def checkTxsStatus(txs: Seq[Transaction], expectedStatus: String, route: Route): Unit = {
      txs.foreach { tx =>
        Get(s"/transactions/status/${tx.id()}") ~> route ~> check {
          (responseAs[JsObject] \ "status").as[String] shouldBe expectedStatus
        }

        Post("/transactions/status", FormData("id" -> tx.id().toString)) ~> route ~> check {
          (responseAs[JsArray].value.head.as[JsObject] \ "status").as[String] shouldBe expectedStatus
        }

        Post(
          "/transactions/status",
          HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> Json.arr(tx.id().toString)).toString())
        ) ~> route ~> check {
          (responseAs[JsArray].value.head.as[JsObject] \ "status").as[String] shouldBe expectedStatus
        }
      }
    }

    val challengedMiner = TxHelpers.signer(1)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(defaultSigner)) { d =>
      val channels      = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
      val promise       = Promise[Unit]()
      val lockChallenge = new ReentrantLock()
      lockChallenge.lock()

      val challengingMiner = d.wallet.generateNewAccount().get

      d.appendBlock(
        TxHelpers.transfer(defaultSigner, challengingMiner.toAddress, 10000.waves),
        TxHelpers.transfer(defaultSigner, challengedMiner.toAddress, 10000.waves)
      )

      (1 to 999).foreach(_ => d.appendBlock())

      val txs = Seq(TxHelpers.transfer(amount = 1.waves), TxHelpers.transfer(amount = 2.waves))
      val invalidBlock =
        d.createBlock(Block.ProtoBlockVersion, txs, strictTime = true, generator = challengedMiner, stateHash = Some(Some(invalidStateHash)))

      val blockChallenger: BlockChallenger =
        new BlockChallengerImpl(
          d.blockchain,
          new DefaultChannelGroup(GlobalEventExecutor.INSTANCE),
          d.wallet,
          d.settings,
          testTime,
          d.posSelector,
          Schedulers.singleThread("miner"),
          createBlockAppender(d)
        ) {
          override def pickBestAccount(accounts: Seq[(SeedKeyPair, Long)]): Either[GenericError, (SeedKeyPair, Long)] = {
            promise.success(())
            lockChallenge.lock()
            val best = super.pickBestAccount(accounts)
            testTime.setTime(invalidBlock.header.timestamp.max(best.explicitGet()._2 + d.lastBlock.header.timestamp))
            best
          }
        }
      val appender =
        BlockAppender(d.blockchain, testTime, d.utxPool, d.posSelector, channels, PeerDatabase.NoOp, Some(blockChallenger), appenderScheduler) _

      val route = new TransactionsApiRoute(
        d.settings.restAPISettings,
        d.commonApi.commonTransactionsApi(blockChallenger),
        d.wallet,
        d.blockchain,
        () => d.blockchain,
        () => 0,
        (t, _) => d.commonApi.transactions.broadcastTransaction(t),
        testTime,
        new RouteTimeout(60.seconds)(sharedScheduler)
      ).route

      testTime.setTime(invalidBlock.header.timestamp)
      val challengeResult = appender(new EmbeddedChannel(), invalidBlock, None).runToFuture

      Await.ready(
        promise.future.map(_ => checkTxsStatus(txs, TransactionsApiRoute.Status.Confirmed, route))(monix.execution.Scheduler.Implicits.global),
        1.minute
      )

      lockChallenge.unlock()

      Await.ready(challengeResult, 1.minute)

      checkTxsStatus(txs, TransactionsApiRoute.Status.Confirmed, route)
    }
  }

  property("NODE-920. Challenging block signature check should fail when challenged header is replaced") {
    def createInvalidChallengingBlock(validChallengingBlock: Block, f: ChallengedHeader => ChallengedHeader): Block = {
      val validChallengedHeader = validChallengingBlock.header.challengedHeader.get

      validChallengingBlock.copy(header = validChallengingBlock.header.copy(challengedHeader = Some(f(validChallengedHeader))))
    }

    withDomain(settings, balances = AddrWithBalance.enoughBalances(defaultSigner)) { d =>
      val challengingMiner      = d.wallet.generateNewAccount().get
      val originalBlock         = d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, stateHash = Some(Some(invalidStateHash)))
      val validChallengingBlock = d.createChallengingBlock(challengingMiner, originalBlock)

      validChallengingBlock.signatureValid() shouldBe true

      val validChallengedHeader = validChallengingBlock.header.challengedHeader.get

      createInvalidChallengingBlock(validChallengingBlock, _.copy(timestamp = validChallengedHeader.timestamp + 1)).signatureValid() shouldBe false
      createInvalidChallengingBlock(validChallengingBlock, _.copy(baseTarget = validChallengedHeader.baseTarget + 1)).signatureValid() shouldBe false
      createInvalidChallengingBlock(
        validChallengingBlock,
        _.copy(generationSignature = ByteStr.fill(validChallengedHeader.generationSignature.size)(1))
      ).signatureValid() shouldBe false
      createInvalidChallengingBlock(validChallengingBlock, _.copy(featureVotes = Seq(1))).signatureValid() shouldBe false
      createInvalidChallengingBlock(validChallengingBlock, _.copy(generator = TxHelpers.signer(100).publicKey)).signatureValid() shouldBe false
      createInvalidChallengingBlock(validChallengingBlock, _.copy(rewardVote = Long.MaxValue)).signatureValid() shouldBe false
      createInvalidChallengingBlock(validChallengingBlock, _.copy(stateHash = Some(ByteStr.fill(DigestLength)(2)))).signatureValid() shouldBe false
      createInvalidChallengingBlock(validChallengingBlock, _.copy(headerSignature = ByteStr.fill(validChallengedHeader.headerSignature.size)(1)))
        .signatureValid() shouldBe false
    }
  }

  property("NODE-934. Block with better timestamp should replace current liquid block regardless it is challenging or not") {
    val challengedMiner    = TxHelpers.signer(1)
    val sender             = TxHelpers.signer(2)
    val currentBlockSender = TxHelpers.signer(3)
    val bestBlockSender    = TxHelpers.signer(4)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender, currentBlockSender, bestBlockSender)) { d =>
      val challengingMiner       = d.wallet.generateNewAccount().get
      val betterChallengingMiner = d.wallet.generateNewAccount().get
      d.appendBlock(
        TxHelpers.transfer(sender, challengingMiner.toAddress, 1000.waves),
        TxHelpers.transfer(sender, betterChallengingMiner.toAddress, 1000.waves),
        TxHelpers.transfer(sender, challengedMiner.toAddress, 2000.waves)
      )
      (1 to 999).foreach(_ => d.appendBlock())

      val txs       = Seq(TxHelpers.transfer(sender, TxHelpers.defaultAddress, amount = 1.waves))
      val bestBlock = d.createBlock(Block.ProtoBlockVersion, txs, generator = bestBlockSender)
      val originalBlock =
        d.createBlock(
          Block.ProtoBlockVersion,
          txs,
          generator = challengedMiner,
          stateHash = Some(Some(invalidStateHash))
        )

      val betterChallengingBlock = d.createChallengingBlock(betterChallengingMiner, originalBlock)
      val worseChallengingBlock  = d.createChallengingBlock(challengingMiner, originalBlock)
      val currentBlock           = d.createBlock(Block.ProtoBlockVersion, txs, generator = currentBlockSender)

      bestBlock.header.timestamp < betterChallengingBlock.header.timestamp shouldBe true
      betterChallengingBlock.header.timestamp < worseChallengingBlock.header.timestamp shouldBe true
      worseChallengingBlock.header.timestamp < currentBlock.header.timestamp shouldBe true

      d.appendBlockE(currentBlock) should beRight
      val expectedHeight = d.blockchain.height

      // replace block without challenge with challenging block
      d.appendBlockE(worseChallengingBlock) should beRight
      d.lastBlock shouldBe worseChallengingBlock
      d.blockchain.height shouldBe expectedHeight

      d.appendBlockE(currentBlock) should produce(
        s"Competitors liquid block $currentBlock(timestamp=${currentBlock.header.timestamp}) is not better than existing"
      )
      d.lastBlock shouldBe worseChallengingBlock
      d.blockchain.height shouldBe expectedHeight

      // replace challenging block with better challenging block
      d.appendBlockE(betterChallengingBlock) should beRight
      d.lastBlock shouldBe betterChallengingBlock
      d.blockchain.height shouldBe expectedHeight

      d.appendBlockE(worseChallengingBlock) should produce(
        s"Competitors liquid block $worseChallengingBlock(timestamp=${worseChallengingBlock.header.timestamp}) is not better than existing"
      )
      d.lastBlock shouldBe betterChallengingBlock
      d.blockchain.height shouldBe expectedHeight

      // replace challenging block with block without challenge
      d.appendBlockE(bestBlock) should beRight
      d.lastBlock shouldBe bestBlock
      d.blockchain.height shouldBe expectedHeight

      d.appendBlockE(betterChallengingBlock) should produce(
        s"Competitors liquid block $betterChallengingBlock(timestamp=${betterChallengingBlock.header.timestamp}) is not better than existing"
      )
      d.lastBlock shouldBe bestBlock
      d.blockchain.height shouldBe expectedHeight
    }
  }

  private def appendAndCheck(block: Block, d: Domain)(check: Block => Unit): Unit = {
    val channels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    val channel1 = new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp))
    val channel2 = new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp))
    channels.add(channel1)
    channels.add(channel2)
    val appenderWithChallenger: Block => Task[Unit] =
      BlockAppender(
        d.blockchain,
        testTime,
        d.utxPool,
        d.posSelector,
        channels,
        PeerDatabase.NoOp,
        Some(createBlockChallenger(d, channels)),
        appenderScheduler
      )(channel2, _, None)

    testTime.setTime(d.blockchain.lastBlockTimestamp.get + d.settings.blockchainSettings.genesisSettings.averageBlockDelay.toMillis * 2)
    appenderWithChallenger(block).runSyncUnsafe()
    if (!channel1.outboundMessages().isEmpty)
      check(PBBlockSpec.deserializeData(channel1.readOutbound[RawBytes]().data).get)
    else fail("block should be defined")
  }

  private def createBlockAppender(d: Domain): Block => Task[Either[ValidationError, BlockApplyResult]] =
    BlockAppender(d.blockchain, testTime, d.utxPool, d.posSelector, appenderScheduler)(_, None)

  private def createMicroBlockAppender(d: Domain): (Channel, MicroBlock) => Task[Unit] = { (ch, mb) =>
    val channels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)

    MicroblockAppender(d.blockchain, d.utxPool, channels, PeerDatabase.NoOp, Some(createBlockChallenger(d, channels)), appenderScheduler)(
      ch,
      MicroblockData(None, mb, Coeval.now(Set.empty)),
      None
    )
  }

  private def createBlockChallenger(d: Domain, allChannels: ChannelGroup = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)): BlockChallenger =
    new BlockChallengerImpl(
      d.blockchain,
      allChannels,
      d.wallet,
      d.settings,
      testTime,
      d.posSelector,
      Schedulers.singleThread("miner"),
      createBlockAppender(d)
    )

  private def checkBlockJson(blockJson: JsObject, sourceBlock: Block) = {
    val chHeader = sourceBlock.header.challengedHeader.get

    (blockJson \ "version").as[Byte] shouldBe sourceBlock.header.version
    (blockJson \ "timestamp").as[Long] shouldBe sourceBlock.header.timestamp
    (blockJson \ "reference").as[String] shouldBe sourceBlock.header.reference.toString
    (blockJson \ "nxt-consensus" \ "base-target").as[Long] shouldBe sourceBlock.header.baseTarget
    (blockJson \ "nxt-consensus" \ "generation-signature").as[String] shouldBe sourceBlock.header.generationSignature.toString
    (blockJson \ "transactionsRoot").as[String] shouldBe sourceBlock.header.transactionsRoot.toString
    (blockJson \ "id").as[String] shouldBe sourceBlock.id().toString
    (blockJson \ "features").as[Seq[Short]] shouldBe sourceBlock.header.featureVotes
    (blockJson \ "desiredReward").as[Long] shouldBe sourceBlock.header.rewardVote
    (blockJson \ "generator").as[String] shouldBe sourceBlock.header.generator.toAddress.toString
    (blockJson \ "generatorPublicKey").as[String] shouldBe sourceBlock.header.generator.toString
    (blockJson \ "stateHash").as[String] shouldBe sourceBlock.header.stateHash.get.toString
    (blockJson \ "challengedHeader" \ "headerSignature").as[String] shouldBe chHeader.headerSignature.toString
    (blockJson \ "challengedHeader" \ "features").as[Seq[Short]] shouldBe chHeader.featureVotes
    (blockJson \ "challengedHeader" \ "generator").as[String] shouldBe chHeader.generator.toAddress.toString
    (blockJson \ "challengedHeader" \ "generatorPublicKey").as[String] shouldBe chHeader.generator.toString
    (blockJson \ "challengedHeader" \ "desiredReward").as[Long] shouldBe chHeader.rewardVote
    (blockJson \ "challengedHeader" \ "stateHash").as[String] shouldBe chHeader.stateHash.get.toString
  }

  private def rollbackMiddleScenario(d: Domain, challengedMiner: KeyPair, txs: Seq[Transaction]): Assertion = {
    (1 to 5).foreach(_ => d.appendBlock())

    val originalBlock = d.createBlock(
      Block.ProtoBlockVersion,
      txs,
      strictTime = true,
      generator = challengedMiner,
      stateHash = Some(Some(invalidStateHash))
    )

    appendAndCheck(originalBlock, d)(_ => (1 to 10).foreach(_ => d.appendBlock()))

    d.blockchain.height shouldBe 1017
  }

  private def rollbackLastScenario(d: Domain, challengedMiner: KeyPair, txs: Seq[Transaction]): Assertion = {
    (1 to 5).foreach(_ => d.appendBlock())

    val originalBlock = d.createBlock(
      Block.ProtoBlockVersion,
      txs,
      strictTime = true,
      generator = challengedMiner,
      stateHash = Some(Some(invalidStateHash))
    )

    appendAndCheck(originalBlock, d)(_ => ())

    d.blockchain.height shouldBe 1007
  }

  private def rollbackActivationHeightScenario(d: Domain, challengedMiner: KeyPair, txs: Seq[Transaction]): Assertion = {
    (1 to 6).foreach(_ => d.appendBlock())

    d.blockchain.isFeatureActivated(BlockchainFeatures.TransactionStateSnapshot) shouldBe false

    val originalBlock = d.createBlock(
      Block.ProtoBlockVersion,
      txs,
      strictTime = true,
      generator = challengedMiner,
      stateHash = Some(Some(invalidStateHash))
    )

    appendAndCheck(originalBlock, d)(_ => ())

    d.blockchain.height shouldBe 1008
  }

  private def getLastBlockMinerReward(d: Domain): Long =
    getLastBlockRewards(d).miner

  private def getLastBlockRewards(d: Domain): BlockRewardShares =
    BlockRewardCalculator
      .getBlockRewardShares(
        d.blockchain.height,
        d.blockchain.settings.rewardsSettings.initial,
        d.blockchain.settings.functionalitySettings.daoAddressParsed.toOption.flatten,
        d.blockchain.settings.functionalitySettings.daoAddressParsed.toOption.flatten,
        d.blockchain
      )
}
