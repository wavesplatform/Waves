package com.wavesplatform.it.sync.lightnode

import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.FairPoSCalculator
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.api.AsyncNetworkApi.NodeAsyncNetworkApi
import com.wavesplatform.it.api.Block as ApiBlock
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.{BaseFunSuite, Node, NodeConfigs, TransferSending}
import com.wavesplatform.lang.directives.values.V8
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.network.RawBytes
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.OrderType
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.{Transaction, TxHelpers, TxNonNegativeAmount}
import com.wavesplatform.{TestValues, crypto}

import scala.concurrent.Await
import scala.concurrent.duration.*

class BlockChallengeSuite extends BaseFunSuite with TransferSending {
  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(4))
      .overrideBase(_.minAssetInfoUpdateInterval(0))
      .overrideBase(
        _.preactivatedFeatures(
          BlockchainFeatures.SynchronousCalls.id.toInt         -> 0,
          BlockchainFeatures.RideV6.id.toInt                   -> 0,
          BlockchainFeatures.ConsensusImprovements.id.toInt    -> 0,
          BlockchainFeatures.LightNode.id.toInt -> 0
        )
      )
      .withDefault(1)
      .withSpecial(1, _.lightNode)
      .withSpecial(2, _.nonMiner)
      .buildNonConflicting()

  test("NODE-1167, NODE-1174, NODE-1175. All nodes should receive and apply block with challenge") {
    val challenger = nodes.head
    val sender     = nodes(1)
    val malicious  = nodes.last

    val height    = challenger.height
    val lastBlock = challenger.blockAt(height)

    val elidedTransfer = TxHelpers.transfer(malicious.keyPair, amount = malicious.balance(malicious.address).balance + 200000000)
    val txs            = createTxs(challenger, sender, nodes(2)) :+ elidedTransfer
    val invalidBlock   = createBlockWithInvalidStateHash(lastBlock, height, malicious.keyPair, txs)
    waitForBlockTime(invalidBlock)
    Await.ready(challenger.sendByNetwork(RawBytes.fromBlock(invalidBlock)), 2.minutes)

    txs.foreach { tx =>
      val txInfo         = nodes.waitForTransaction(tx.id().toString)
      val expectedStatus = if (tx.id() == elidedTransfer.id()) "elided" else "succeeded"
      txInfo.applicationStatus shouldBe Some(expectedStatus)
    }

    val challengingIds = nodes.map { node =>
      val challengingBlock = node.blockAt(height + 1)
      checkChallengingBlock(challengingBlock, invalidBlock, challenger.address, txs)
      challengingBlock.id
    }

    challengingIds.toSet.size shouldBe 1
  }

  private def checkChallengingBlock(challengingBlock: ApiBlock, challengedBlock: Block, challengerAddress: String, txs: Seq[Transaction]) = {
    challengingBlock.challengedHeader shouldBe defined
    val challengedHeader = challengingBlock.challengedHeader.get
    challengedHeader.headerSignature shouldBe challengedBlock.signature.toString
    challengedHeader.features shouldBe challengedBlock.header.featureVotes.toSet
    challengedHeader.desiredReward shouldBe challengedBlock.header.rewardVote
    challengedHeader.stateHash shouldBe challengedBlock.header.stateHash.map(_.toString)
    challengedHeader.generator shouldBe challengedBlock.header.generator.toAddress.toString
    challengedHeader.generatorPublicKey shouldBe challengedBlock.header.generator.toString
    challengingBlock.generator shouldBe challengerAddress
    challengingBlock.transactions.map(_.id).toSet shouldBe txs.map(_.id().toString).toSet
  }

  private def createBlockWithInvalidStateHash(lastBlock: ApiBlock, height: Int, signer: KeyPair, txs: Seq[Transaction]): Block = {
    val lastBlockVrfOrGenSig = lastBlock.vrf.orElse(lastBlock.generationSignature).map(str => ByteStr.decodeBase58(str).get).get.arr
    val genSig: ByteStr      = crypto.signVRF(signer.privateKey, lastBlockVrfOrGenSig)

    val hitSource =
      crypto.verifyVRF(genSig, lastBlockVrfOrGenSig, signer.publicKey).explicitGet()

    val posCalculator = FairPoSCalculator.V1
    val version       = 5.toByte

    val validBlockDelay: Long = posCalculator
      .calculateDelay(
        hit(hitSource.arr),
        lastBlock.baseTarget.get,
        nodes.head.accountBalances(signer.toAddress.toString)._2
      )

    val baseTarget: Long = posCalculator
      .calculateBaseTarget(
        10,
        height,
        lastBlock.baseTarget.get,
        lastBlock.timestamp,
        None,
        lastBlock.timestamp + validBlockDelay
      )

    Block
      .buildAndSign(
        version = version,
        timestamp = lastBlock.timestamp + validBlockDelay,
        reference = ByteStr.decodeBase58(lastBlock.id).get,
        baseTarget = baseTarget,
        generationSignature = genSig,
        txs = txs,
        signer = signer,
        featureVotes = Seq(22),
        rewardVote = 1000000000L,
        stateHash = Some(ByteStr.fill(32)(1)),
        challengedHeader = None
      )
      .explicitGet()
  }

  private def hit(generatorSignature: Array[Byte]): BigInt = BigInt(1, generatorSignature.take(8).reverse)

  private def waitForBlockTime(block: Block): Unit = {
    val timeout = block.header.timestamp - System.currentTimeMillis()

    if (timeout > 0) Thread.sleep(timeout)
  }

  private def createTxs(challenger: Node, sender: Node, dApp: Node): Seq[Transaction] = {
    val dAppScript = TestCompiler(V8).compileContract(
      s"""
         |@Callable(i)
         |func foo() = []
         |""".stripMargin
    )
    val assetScript = TestCompiler(V8).compileAsset("true")

    val issue      = TxHelpers.issue(sender.keyPair)
    val issueSmart = TxHelpers.issue(sender.keyPair, name = "smart", script = Some(assetScript))
    val lease      = TxHelpers.lease(sender.keyPair)

    Seq(
      issue,
      issueSmart,
      TxHelpers.setScript(dApp.keyPair, dAppScript),
      TxHelpers.burn(issue.asset, sender = sender.keyPair),
      TxHelpers.createAlias("alias", sender.keyPair),
      TxHelpers.dataSingle(sender.keyPair),
      TxHelpers.exchange(
        TxHelpers.order(OrderType.BUY, issue.asset, Waves, sender = sender.keyPair, matcher = sender.keyPair),
        TxHelpers.order(OrderType.SELL, issue.asset, Waves, sender = sender.keyPair, matcher = sender.keyPair),
        sender.keyPair
      ),
      TxHelpers.invoke(dApp.keyPair.toAddress, Some("foo"), invoker = sender.keyPair),
      lease,
      TxHelpers.leaseCancel(lease.id(), sender.keyPair),
      TxHelpers
        .massTransfer(
          sender.keyPair,
          Seq(ParsedTransfer(challenger.keyPair.toAddress, TxNonNegativeAmount.unsafeFrom(1))),
          fee = TestValues.fee
        ),
      TxHelpers.reissue(issue.asset, sender.keyPair),
      TxHelpers.setAssetScript(sender.keyPair, issueSmart.asset, assetScript, fee = 200000000),
      TxHelpers.transfer(sender.keyPair, challenger.keyPair.toAddress, 1),
      TxHelpers.sponsor(issue.asset, sender = sender.keyPair),
      TxHelpers.updateAssetInfo(issue.assetId, sender = sender.keyPair)
    )
  }
}
