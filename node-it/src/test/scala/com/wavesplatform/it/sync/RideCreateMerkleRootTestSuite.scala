package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.common.merkle.Merkle
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it._
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset._
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransaction
import org.scalatest.{FunSuite, Matchers}

class RideCreateMerkleRootTestSuite extends FunSuite with NodesFromDocker with ReportingTestName with Matchers with NTPTime {
  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(
        _.preactivatedFeatures(
          BlockchainFeatures.BlockReward -> 0,
          BlockchainFeatures.NG -> 0,
          BlockchainFeatures.FairPoS -> 0,
          BlockchainFeatures.Ride4DApps -> 0,
          BlockchainFeatures.BlockV5 -> 0
        )
      )
      .withDefault(1)
      .buildNonConflicting()

  private def miner: Node = nodes.last

  private lazy val scriptedKeyPair = miner.createKeyPair()

  test("Ride createMerkleRoot") {
    val cscript = ScriptCompiler
      .compile(
        """
        |{-# STDLIB_VERSION 4 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |
        | @Callable(inv)
        |func foo(proof: List[ByteVector], id: ByteVector, index: Int, txId: String) = [
        | BinaryEntry(txId, createMerkleRoot(proof, id, index))
        |]
        """.stripMargin,
        ScriptEstimatorV3
      )
      .explicitGet()
      ._1
      .bytes()
      .base64

    val transactions = Seq(setScriptFee, 1, 1, 1, 1).map { amount =>
      TransferTransaction
        .selfSigned(
          3.toByte,
          miner.keyPair,
          scriptedKeyPair.toAddress,
          Waves,
          amount,
          Waves,
          minFee,
          ByteStr.empty,
          ntpTime.getTimestamp()
        )
        .explicitGet()
    }

    transactions.foreach(tx => miner.signedBroadcast(tx.json()))

    val transactionInfo = transactions.map(tx => tx -> miner.waitForTransaction(tx.id().toString))

    miner.waitForHeight(transactionInfo.map(_._2.height).max + 1)
    miner.setScript(scriptedKeyPair, Some(cscript), setScriptFee, waitForTx = true)

    for (((tx, txInfo), proofs) <- transactionInfo.zip(miner.getMerkleProof(transactionInfo.map(_._2.id): _*))) {
      miner.waitForTransaction(
        miner
          .invokeScript(
            miner.keyPair,
            scriptedKeyPair.toAddress.toString,
            func = Some("foo"),
            args = List(
              ARR(proofs.merkleProof.map(v => CONST_BYTESTR(ByteStr(Base58.decode(v))).explicitGet()).toIndexedSeq, false).explicitGet(),
              CONST_BYTESTR(ByteStr(Merkle.hash(tx.bytes()))).explicitGet(),
              CONST_LONG(proofs.transactionIndex),
              CONST_STRING(txInfo.id).explicitGet()
            ),
            payment = Seq(),
            fee = 2 * smartFee + minFee,
            waitForTx = true
          )
          ._1
          .id
      )

      val root = ByteStr.decodeBase58(miner.blockHeadersAt(txInfo.height).transactionsRoot.getOrElse("")).get

      miner.getDataByKey(scriptedKeyPair.toAddress.toString, txInfo.id) shouldBe BinaryDataEntry(txInfo.id, root)
    }
  }
}
