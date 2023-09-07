package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.account.{AddressScheme, KeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.FairPoSCalculator
import com.wavesplatform.crypto
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.api.Block as ApiBlock
import com.wavesplatform.it.{BaseFunSuite, NodeConfigs, TransferSending}
import com.wavesplatform.it.api.AsyncNetworkApi.NodeAsyncNetworkApi
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.network.RawBytes
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.{Proofs, Transaction, TxPositiveAmount}
import com.wavesplatform.transaction.transfer.TransferTransaction

import scala.concurrent.Await
import scala.concurrent.duration.*

class BlockChallengeSuite extends BaseFunSuite with TransferSending {
  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(4))
      .overrideBase(_.preactivatedFeatures(BlockchainFeatures.TransactionStateSnapshot.id.toInt -> 0))
      .withDefault(1)
      .withSpecial(1, _.lightNode)
      .withSpecial(2, _.nonMiner)
      .buildNonConflicting()

  test("NODE-1167. All nodes should receive and apply block with challenge") {
    val challenger = nodes.head
    val malicious  = nodes.last

    val height    = challenger.height
    val lastBlock = challenger.blockAt(height)

    val txs = (1 to 3).map { idx =>
      TransferTransaction(
        3.toByte,
        challenger.publicKey,
        malicious.keyPair.toAddress,
        Waves,
        TxPositiveAmount.unsafeFrom(100000000 * (idx + 1)),
        Waves,
        TxPositiveAmount.unsafeFrom(1000000),
        ByteStr.empty,
        System.currentTimeMillis(),
        Proofs.empty,
        AddressScheme.current.chainId
      ).signWith(challenger.keyPair.privateKey)
    }
    val invalidBlock = createBlockWithInvalidStateHash(lastBlock, height, malicious.keyPair, txs)
    Await.ready(challenger.sendByNetwork(RawBytes.fromBlock(invalidBlock)), 2.minutes)

    txs.foreach { tx =>
      nodes.waitForTransaction(tx.id().toString)
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

    val posCalculator = FairPoSCalculator.V2
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
}
