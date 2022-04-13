package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.account._
import com.wavesplatform.common.merkle.Merkle
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.Transaction
import com.wavesplatform.it.{BaseFunSuite, NodeConfigs, TransferSending}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset._
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Proofs, TxVersion}
import org.scalatest.prop.TableDrivenPropertyChecks

class RideCreateMerkleRootTestSuite extends BaseFunSuite with TransferSending with TableDrivenPropertyChecks {
  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(
        _.preactivatedFeatures(
          (14, 1000000),
          BlockchainFeatures.NG.id.toInt         -> 0,
          BlockchainFeatures.FairPoS.id.toInt    -> 0,
          BlockchainFeatures.Ride4DApps.id.toInt -> 0,
          BlockchainFeatures.BlockV5.id.toInt    -> 0
        )
      )
      .withDefault(1)
      .buildNonConflicting()

  test("Ride createMerkleRoot") {
    val script  = """
        |{-# STDLIB_VERSION 4 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |
        | @Callable(inv)
        |func foo(proof: List[ByteVector], id: ByteVector, index: Int) = [
        | BinaryEntry("root", createMerkleRoot(proof, id, index))
        |]
        """.stripMargin
    val cscript = ScriptCompiler.compile(script, ScriptEstimatorV3(fixOverflow = true)).explicitGet()._1.bytes().base64
    val node    = nodes.head
    nodes.waitForHeightArise()
    val tx1   = node.broadcastTransfer(node.keyPair, sender.address, setScriptFee, minFee, None, None, version = TxVersion.V3, waitForTx = false)
    val txId1 = tx1.id
    val tx2   = node.broadcastTransfer(node.keyPair, node.address, 1, minFee, None, None, version = TxVersion.V3, waitForTx = false)
    val txId2 = tx2.id
    val tx3   = node.broadcastTransfer(node.keyPair, node.address, 1, minFee, None, None, version = TxVersion.V3, waitForTx = false)
    val txId3 = tx3.id
    val tx4   = node.broadcastTransfer(node.keyPair, node.address, 1, minFee, None, None, version = TxVersion.V3, waitForTx = false)
    val txId4 = tx4.id
    val tx5   = node.broadcastTransfer(node.keyPair, node.address, 1, minFee, None, None, version = TxVersion.V3, waitForTx = false)
    val txId5 = tx5.id

    val height = node.height

    nodes.waitForHeightArise()

    def tt(tx: Transaction) =
      TransferTransaction
        .create(
          tx.version.get,
          PublicKey(Base58.decode(tx.senderPublicKey.get)),
          Address.fromString(tx.recipient.get).explicitGet(),
          Waves /* not support tx.asset.fold(Waves)(v => IssuedAsset(Base58.decode(v))) */,
          tx.amount.get,
          Waves /* not support tx.feeAsset.fold(Waves)(v => Issued(Base58.decode(v))) */,
          tx.fee,
          ByteStr.empty, // attachment
          tx.timestamp,
          Proofs(tx.proofs.get.map(v => ByteStr(Base58.decode(v))))
        )
        .explicitGet()
    val natives = Seq(tx1, tx2, tx3, tx4, tx5).map(tt).map(t => Base58.encode(t.id().arr) -> t).toMap

    val root = Base58.decode(node.blockAt(height).transactionsRoot.get)

    val proofs = nodes.head.getMerkleProof(txId1, txId2, txId3, txId4, txId5)

    sender.setScript(sender.keyPair, Some(cscript), setScriptFee, waitForTx = true).id

    for (p <- proofs) {
      node.invokeScript(
        node.keyPair,
        sender.address,
        func = Some("foo"),
        args = List(
          ARR(p.merkleProof.map(v => CONST_BYTESTR(ByteStr(Base58.decode(v))).explicitGet()).toIndexedSeq, false).explicitGet(),
          CONST_BYTESTR(ByteStr(Merkle.hash(natives(p.id).bytes()))).explicitGet(),
          CONST_LONG(p.transactionIndex.toLong)
        ),
        payment = Seq(),
        fee = 2 * smartFee + minFee,
        waitForTx = true
      )
      node.getDataByKey(sender.address, "root") shouldBe BinaryDataEntry("root", ByteStr(root))
    }
  }
}
