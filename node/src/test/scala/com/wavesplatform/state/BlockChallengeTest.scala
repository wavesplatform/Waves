package com.wavesplatform.state

import akka.http.scaladsl.model.{ContentTypes, FormData, HttpEntity}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.*
import com.wavesplatform.TestValues
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.http.TransactionsApiRoute.{ApplicationStatus, Status}
import com.wavesplatform.api.http.{ApiMarshallers, BlocksApiRoute, RouteTimeout, TransactionsApiRoute}
import com.wavesplatform.block.{Block, ChallengedHeader}
import com.wavesplatform.common.merkle.Merkle
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.{Domain, defaultSigner}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, CONST_LONG}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.mining.BlockChallenger
import com.wavesplatform.network.{MessageCodec, PBBlockSpec, PeerDatabase, RawBytes}
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.transaction.{Transaction, TxHelpers}
import com.wavesplatform.transaction.TxValidationError.{BlockAppendError, GenericError, MicroBlockAppendError}
import com.wavesplatform.utils.{JsonMatchers, Schedulers}
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.group.{ChannelGroup, DefaultChannelGroup}
import io.netty.util.concurrent.GlobalEventExecutor
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.Assertion
import play.api.libs.json.*

import scala.concurrent.duration.DurationInt

class BlockChallengeTest extends PropSpec with WithDomain with ScalatestRouteTest with ApiMarshallers with JsonMatchers {

  implicit val appenderScheduler: Scheduler = Scheduler.singleThread("appender")
  val settings: WavesSettings =
    DomainPresets.TransactionStateSnapshot.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  val testTime: TestTime = TestTime()

  property("NODE-883. Invalid challenging block should be ignored") {
    val sender           = TxHelpers.signer(1)
    val challengedMiner  = TxHelpers.signer(2)
    val challengingMiner = TxHelpers.signer(3)
    withDomain(settings, balances = AddrWithBalance.enoughBalances(sender)) { d =>
      d.appendBlock()
      val invalidStateHash = ByteStr.fill(DigestLength)(1)
      val txs              = Seq(TxHelpers.transfer(sender, amount = 1), TxHelpers.transfer(sender, amount = 2))
      val challengedBlock  = d.createBlock(Block.ProtoBlockVersion, txs, generator = challengedMiner, stateHash = Some(Some(invalidStateHash)))
      val invalidHashChallengingBlock = createChallengingBlock(d, challengingMiner, challengedBlock, Some(Some(invalidStateHash)))
      val missedHashChallengingBlock  = createChallengingBlock(d, challengingMiner, challengedBlock, Some(None))

      d.appendBlockE(invalidHashChallengingBlock) shouldBe Left(GenericError("Invalid block challenge"))
      d.appendBlockE(missedHashChallengingBlock) shouldBe Left(GenericError("Invalid block challenge"))
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
      val invalidStateHash = ByteStr.fill(DigestLength)(1)
      val originalBlock =
        d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, generator = challengedMiner, stateHash = Some(Some(invalidStateHash)))
      val challengingBlock = createChallengingBlock(d, challengingMiner, originalBlock)

      val challengingGenBalanceBefore = d.blockchain.generatingBalance(challengingMiner.toAddress, Some(challengingBlock.header.reference))
      val challengingEffBalanceBefore = d.blockchain.effectiveBalance(challengingMiner.toAddress, 0)
      val challengedGenBalanceBefore  = d.blockchain.generatingBalance(challengedMiner.toAddress, Some(challengingBlock.header.reference))

      d.appendBlockE(challengingBlock) should beRight
      d.blockchain.generatingBalance(
        challengingMiner.toAddress,
        Some(challengingBlock.header.reference)
      ) shouldBe challengingGenBalanceBefore + challengedGenBalanceBefore
      d.blockchain.effectiveBalance(
        challengingMiner.toAddress,
        0
      ) shouldBe challengingEffBalanceBefore + d.blockchain.settings.rewardsSettings.initial

      d.blockchain.generatingBalance(challengingMiner.toAddress, Some(challengingBlock.id())) shouldBe challengingGenBalanceBefore
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
      val invalidStateHash = ByteStr.fill(DigestLength)(1)
      val originalBlock    = d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, stateHash = Some(Some(invalidStateHash)))
      appendAndCheck(originalBlock, d) {
        case Some(block) =>
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
        case _ => fail("block should be defined")
      }
    }
  }

  property(
    s"NODE-889. Block without challenge (before ${BlockchainFeatures.TransactionStateSnapshot} activation) should not contain ChallengedHeader"
  ) {
    val sender           = TxHelpers.signer(1)
    val challengedMiner  = TxHelpers.signer(2)
    val challengingMiner = TxHelpers.signer(3)
    withDomain(DomainPresets.ConsensusImprovements, balances = AddrWithBalance.enoughBalances(sender, challengedMiner, challengingMiner)) { d =>
      d.appendBlock()
      val invalidStateHash = ByteStr.fill(DigestLength)(1)
      val txs              = Seq(TxHelpers.transfer(sender, amount = 1), TxHelpers.transfer(sender, amount = 2))
      val challengedBlock =
        d.createBlock(Block.ProtoBlockVersion, txs, strictTime = true, generator = challengedMiner, stateHash = Some(Some(invalidStateHash)))
      val blockWithChallenge = createChallengingBlock(d, challengingMiner, challengedBlock, Some(Some(invalidStateHash)))

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
      val invalidStateHash = ByteStr.fill(DigestLength)(1)
      val originalBlock    = d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, stateHash = Some(Some(invalidStateHash)))
      appendAndCheck(originalBlock, d) {
        case Some(block) =>
          block.transactionData shouldBe originalBlock.transactionData
        case _ => fail("block should be defined")
      }
    }
  }

  property("NODE-891. Challenging block should contain only transactions from original block") {
    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get
      d.appendBlock(TxHelpers.transfer(TxHelpers.defaultSigner, challengingMiner.toAddress, 1000.waves))
      (1 to 999).foreach(_ => d.appendBlock())
      val invalidStateHash = ByteStr.fill(DigestLength)(1)
      val originalBlock    = d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, stateHash = Some(Some(invalidStateHash)))
      val invalidChallengingBlock = createChallengingBlock(
        d,
        challengingMiner,
        originalBlock,
        None,
        txs = Some(originalBlock.transactionData :+ TxHelpers.transfer(TxHelpers.defaultSigner))
      )

      d.appendBlockE(invalidChallengingBlock) shouldBe Left(GenericError(s"Block $invalidChallengingBlock has invalid signature"))

      val correctChallengingBlock = createChallengingBlock(d, challengingMiner, originalBlock, None)
      d.appendBlockE(correctChallengingBlock) should beRight

      val microblock = d.createMicroBlock(None, Some(challengingMiner), TxHelpers.transfer(TxHelpers.defaultSigner))
      d.appendMicroBlockE(microblock) shouldBe Left(MicroBlockAppendError("Base block has challenged header", microblock))
    }
  }

  property("NODE-892. Challenging block should reference the same block as original") {
    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get
      d.appendBlock(TxHelpers.transfer(TxHelpers.defaultSigner, challengingMiner.toAddress, 1000.waves))
      (1 to 999).foreach(_ => d.appendBlock())
      val invalidStateHash = ByteStr.fill(DigestLength)(1)
      val originalBlock    = d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, stateHash = Some(Some(invalidStateHash)))
      appendAndCheck(originalBlock, d) {
        case Some(block) =>
          block.header.reference shouldBe originalBlock.header.reference
        case _ => fail("block should be defined")
      }
    }
  }

  property("NODE-893. Challenging block can't reference blocks before previous") {
    withDomain(settings, balances = AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)) { d =>
      val challengingMiner = d.wallet.generateNewAccount().get
      d.appendBlock(TxHelpers.transfer(TxHelpers.defaultSigner, challengingMiner.toAddress, 1000.waves))
      (1 to 999).foreach(_ => d.appendBlock())
      val invalidStateHash = ByteStr.fill(DigestLength)(1)
      val originalBlock    = d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, stateHash = Some(Some(invalidStateHash)))
      val grandParent      = d.blockchain.lastBlockHeader.map(_.header.reference)
      val challengingBlock = createChallengingBlock(d, challengingMiner, originalBlock, None, grandParent)

      d.appendBlockE(challengingBlock) shouldBe Left(BlockAppendError("References incorrect or non-existing block", challengingBlock))
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
      val invalidStateHash = ByteStr.fill(DigestLength)(1)
      val originalBlock =
        d.createBlock(
          Block.ProtoBlockVersion,
          Seq(TxHelpers.transfer(challengedMiner, TxHelpers.defaultAddress, amount = 1.waves)),
          strictTime = true,
          generator = challengedMiner,
          stateHash = Some(Some(invalidStateHash))
        )
      val challengingBlock = createChallengingBlock(d, challengingMiner, originalBlock)

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

      val invalidStateHash   = ByteStr.fill(DigestLength)(1)
      val challengedBlockTxs = Seq(TxHelpers.transfer(sender), TxHelpers.transfer(sender))
      val originalBlock      = d.createBlock(Block.ProtoBlockVersion, challengedBlockTxs, strictTime = true, stateHash = Some(Some(invalidStateHash)))
      val originalMinerBalance    = d.balance(originalBlock.header.generator.toAddress)
      val challengingMinerBalance = d.balance(challengingMiner.toAddress)
      val challengingBlock        = createChallengingBlock(d, challengingMiner, originalBlock)

      d.appendBlockE(challengingBlock) should beRight

      d.balance(originalBlock.header.generator.toAddress) shouldBe originalMinerBalance
      d.balance(
        challengingMiner.toAddress
      ) shouldBe challengingMinerBalance + d.settings.blockchainSettings.rewardsSettings.initial + (prevBlockTx.fee.value - BlockDiffer
        .CurrentBlockFeePart(prevBlockTx.fee.value)) + challengedBlockTxs.map(tx => BlockDiffer.CurrentBlockFeePart(tx.fee.value)).sum
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

      val invalidStateHash  = ByteStr.fill(DigestLength)(1)
      val challengedBlockTx = TxHelpers.transfer(challengedMiner, amount = 1005.waves)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq(challengedBlockTx),
        strictTime = true,
        generator = challengedMiner,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = createChallengingBlock(d, challengingMiner, originalBlock)

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

      val invalidStateHash  = ByteStr.fill(DigestLength)(1)
      val challengedBlockTx = TxHelpers.transfer(challengedMiner, amount = 10005.waves)
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

          d.appendBlock(TxHelpers.transfer(sender, challengedMiner.toAddress, 10.waves))
          d.transactionDiffer(challengedBlockTx).resultE should produce("AlreadyInTheState")
        case _ => fail("block should be defined")
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

      val invalidStateHash  = ByteStr.fill(DigestLength)(1)
      val challengedBlockTx = TxHelpers.transfer(challengedMiner, amount = 1005.waves)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq(challengedBlockTx),
        strictTime = true,
        generator = challengedMiner,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = createChallengingBlock(d, challengingMiner, originalBlock)

      d.appendBlockE(challengingBlock) should beRight
      d.transactionsApi.transactionById(challengedBlockTx.id()).map(_.status).contains(TxMeta.Status.Elided) shouldBe true

      val route = new TransactionsApiRoute(
        d.settings.restAPISettings,
        d.commonApi.transactions,
        d.wallet,
        d.blockchain,
        () => d.blockchain.getCompositeBlockchain,
        () => 0,
        (t, _) => d.commonApi.transactions.broadcastTransaction(t),
        testTime,
        new RouteTimeout(60.seconds)(Schedulers.fixedPool(1, "heavy-request-scheduler"))
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

      val invalidStateHash  = ByteStr.fill(DigestLength)(1)
      val challengedBlockTx = TxHelpers.transfer(challengedMiner, amount = 1005.waves)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq(challengedBlockTx),
        strictTime = true,
        generator = challengedMiner,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = createChallengingBlock(d, challengingMiner, originalBlock)

      d.appendBlockE(challengingBlock) should beRight
      d.transactionsApi.transactionById(challengedBlockTx.id()).map(_.status).contains(TxMeta.Status.Elided) shouldBe true

      d.appendBlock(TxHelpers.invoke(dApp.toAddress, Some("foo"), Seq(CONST_BYTESTR(challengedBlockTx.id()).explicitGet()), invoker = sender))
      d.blockchain.accountData(dApp.toAddress, "check") shouldBe Some(BooleanDataEntry("check", true))
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

      val invalidStateHash = ByteStr.fill(DigestLength)(1)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq.empty,
        strictTime = true,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = createChallengingBlock(d, challengingMiner, originalBlock)

      d.appendBlockE(challengingBlock) should beRight
      val vrf = d.blockchain.vrf(1002).get

      val script =
        TestCompiler(V6).compileContract(
          s"""
             |@Callable(i)
             |func foo(h: Int) = {
             |   let blockInfo = value(blockInfoByHeight(h))
             |
             |   [BooleanEntry("check", blockInfo.timestamp == ${challengingBlock.header.timestamp} && blockInfo.baseTarget == ${challengingBlock.header.baseTarget} &&
             |   blockInfo.generationSignature == base58'${challengingBlock.header.generationSignature}' && blockInfo.height == 1002 && blockInfo.generator == Address(base58'${challengingBlock.header.generator.toAddress}') &&
             |   blockInfo.generatorPublicKey == base58'${challengingBlock.header.generator}' && blockInfo.vrf == base58'$vrf')]
             |}
             |
             |""".stripMargin
        )

      d.appendBlock(TxHelpers.setScript(dApp, script))
      d.appendBlock(TxHelpers.invoke(dApp.toAddress, Some("foo"), Seq(CONST_LONG(1002)), invoker = sender))

      d.blockchain.accountData(dApp.toAddress, "check") shouldBe Some(BooleanDataEntry("check", true))
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

      val invalidStateHash = ByteStr.fill(DigestLength)(1)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq.empty,
        strictTime = true,
        stateHash = Some(Some(invalidStateHash))
      )

      appendAndCheck(originalBlock, d) {
        case Some(block) =>
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
        case _ => fail("block should be defined")
      }
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
        Seq.empty,
        strictTime = true
      )

      appendAndCheck(originalBlock, d) {
        case Some(block) =>
          block.header.challengedHeader should not be defined
          block.header.timestamp shouldBe originalBlock.header.timestamp
          block.header.baseTarget shouldBe originalBlock.header.baseTarget
          block.header.generationSignature shouldBe originalBlock.header.generationSignature
          block.header.featureVotes shouldBe originalBlock.header.featureVotes
          block.header.generator shouldBe originalBlock.header.generator
          block.header.rewardVote shouldBe originalBlock.header.rewardVote
          block.header.transactionsRoot shouldBe originalBlock.header.transactionsRoot
          block.header.stateHash shouldBe originalBlock.header.stateHash
        case _ => fail("block should be defined")
      }

      val challengingBlock = createChallengingBlock(d, challengingMiner, originalBlock)
      d.appendBlockE(challengingBlock) should beLeft
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

      val invalidStateHash = ByteStr.fill(DigestLength)(1)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq.empty,
        strictTime = true,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = createChallengingBlock(d, challengingMiner, originalBlock)
      val blockHeight      = 1002

      d.appendBlockE(challengingBlock) should beRight

      val route = new BlocksApiRoute(
        d.settings.restAPISettings,
        d.blocksApi,
        testTime,
        new RouteTimeout(60.seconds)(Schedulers.fixedPool(1, "heavy-request-scheduler"))
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

      val invalidStateHash  = ByteStr.fill(DigestLength)(1)
      val challengedBlockTx = TxHelpers.transfer(challengedMiner, amount = 1005.waves)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq(challengedBlockTx),
        strictTime = true,
        generator = challengedMiner,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = createChallengingBlock(d, challengingMiner, originalBlock)

      d.appendBlockE(challengingBlock) should beRight
      d.transactionsApi.transactionById(challengedBlockTx.id()).map(_.status).contains(TxMeta.Status.Elided) shouldBe true

      val route = new TransactionsApiRoute(
        d.settings.restAPISettings,
        d.commonApi.transactions,
        d.wallet,
        d.blockchain,
        () => d.blockchain.getCompositeBlockchain,
        () => 0,
        (t, _) => d.commonApi.transactions.broadcastTransaction(t),
        testTime,
        new RouteTimeout(60.seconds)(Schedulers.fixedPool(1, "heavy-request-scheduler"))
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

      val invalidStateHash  = ByteStr.fill(DigestLength)(1)
      val challengedBlockTx = TxHelpers.transfer(challengedMiner, amount = 1005.waves)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq(challengedBlockTx),
        strictTime = true,
        generator = challengedMiner,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = createChallengingBlock(d, challengingMiner, originalBlock)

      d.appendBlockE(challengingBlock) should beRight
      d.transactionsApi.transactionById(challengedBlockTx.id()).map(_.status).contains(TxMeta.Status.Elided) shouldBe true

      val route = new TransactionsApiRoute(
        d.settings.restAPISettings,
        d.commonApi.transactions,
        d.wallet,
        d.blockchain,
        () => d.blockchain.getCompositeBlockchain,
        () => 0,
        (t, _) => d.commonApi.transactions.broadcastTransaction(t),
        testTime,
        new RouteTimeout(60.seconds)(Schedulers.fixedPool(1, "heavy-request-scheduler"))
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

      val invalidStateHash  = ByteStr.fill(DigestLength)(1)
      val challengedBlockTx = TxHelpers.transfer(challengedMiner, amount = 1005.waves)
      val originalBlock = d.createBlock(
        Block.ProtoBlockVersion,
        Seq(challengedBlockTx),
        strictTime = true,
        generator = challengedMiner,
        stateHash = Some(Some(invalidStateHash))
      )
      val challengingBlock = createChallengingBlock(d, challengingMiner, originalBlock)

      d.appendBlockE(challengingBlock) should beRight
      d.transactionsApi.transactionById(challengedBlockTx.id()).map(_.status).contains(TxMeta.Status.Elided) shouldBe true

      val route = new TransactionsApiRoute(
        d.settings.restAPISettings,
        d.commonApi.transactions,
        d.wallet,
        d.blockchain,
        () => d.blockchain.getCompositeBlockchain,
        () => 0,
        (t, _) => d.commonApi.transactions.broadcastTransaction(t),
        testTime,
        new RouteTimeout(60.seconds)(Schedulers.fixedPool(1, "heavy-request-scheduler"))
      ).route

      checkTxStatus(challengedBlockTx, 0, route)
      d.appendBlock()
      checkTxStatus(challengedBlockTx, 1, route)
    }
  }

  property("NODE-920. Challenging block signature check should fail when challenged header is replaced") {
    def createInvalidChallengingBlock(validChallengingBlock: Block, f: ChallengedHeader => ChallengedHeader): Block = {
      val validChallengedHeader = validChallengingBlock.header.challengedHeader.get

      validChallengingBlock.copy(header = validChallengingBlock.header.copy(challengedHeader = Some(f(validChallengedHeader))))
    }

    withDomain(settings, balances = AddrWithBalance.enoughBalances(defaultSigner)) { d =>
      val challengingMiner      = d.wallet.generateNewAccount().get
      val invalidStateHash      = ByteStr.fill(DigestLength)(1)
      val originalBlock         = d.createBlock(Block.ProtoBlockVersion, Seq.empty, strictTime = true, stateHash = Some(Some(invalidStateHash)))
      val validChallengingBlock = createChallengingBlock(d, challengingMiner, originalBlock)

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
      createBlockChallenger(d, channels),
      appenderScheduler
    )(channel2, _)

    testTime.setTime(d.blockchain.lastBlockTimestamp.get + d.settings.blockchainSettings.genesisSettings.averageBlockDelay.toMillis * 2)
    appenderWithChallenger(block).runSyncUnsafe()
    if (!channel1.outboundMessages().isEmpty)
      check(Some(PBBlockSpec.deserializeData(channel1.readOutbound[RawBytes]().data).get))
    else check(None)
  }

  private def createBlockAppender(d: Domain): Block => Task[Either[ValidationError, Option[BigInt]]] =
    BlockAppender(d.blockchain, testTime, d.utxPool, d.posSelector, appenderScheduler)

  private def createBlockChallenger(d: Domain, allChannels: ChannelGroup = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)): BlockChallenger =
    new BlockChallenger(
      d.blockchain,
      allChannels,
      d.wallet,
      d.settings,
      testTime,
      d.posSelector,
      createBlockAppender(d)
    )

  private def createChallengingBlock(
      d: Domain,
      challengingMiner: KeyPair,
      challengedBlock: Block,
      stateHash: Option[Option[ByteStr]] = None,
      ref: Option[ByteStr] = None,
      txs: Option[Seq[Transaction]] = None,
      challengedHeader: Option[ChallengedHeader] = None
  ): Block = {
    d.createBlock(
      Block.ProtoBlockVersion,
      txs.getOrElse(challengedBlock.transactionData),
      ref.orElse(d.blockchain.lastBlockId),
      strictTime = true,
      generator = challengingMiner,
      stateHash = stateHash,
      challengedHeader = Some(
        challengedHeader.getOrElse(
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
    )
  }

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
}
