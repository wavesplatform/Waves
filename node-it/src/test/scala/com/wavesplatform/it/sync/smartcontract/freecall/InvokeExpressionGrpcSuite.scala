package com.wavesplatform.it.sync.smartcontract.freecall

import com.google.protobuf.ByteString
import com.typesafe.config.Config
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.features.BlockchainFeatures.RideV6
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.api.{PutDataResponse, StateChangesDetails}
import com.wavesplatform.it.sync.grpc.GrpcBaseTransactionSuite
import com.wavesplatform.it.sync.invokeExpressionFee
import com.wavesplatform.lang.directives.values.StdLibVersion.V6
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.protobuf.block.VanillaBlock
import com.wavesplatform.transaction.smart.InvokeExpressionTransaction
import com.wavesplatform.transaction.Asset.Waves
import org.scalatest.{Assertion, CancelAfterFailure}

class InvokeExpressionGrpcSuite extends GrpcBaseTransactionSuite with CancelAfterFailure {
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(Default, 2, Seq.empty)
      .overrideBase(_.preactivatedFeatures((RideV6.id, 1)))
      .buildNonConflicting()

  private val expr: ExprScript =
    TestCompiler(V6).compileFreeCall(
      """
        | [
        |   BooleanEntry("check", true)
        | ]
      """.stripMargin
    )

  test("successful applying to the state") {
    val id = sender.broadcastInvokeExpression(firstAcc, expr, waitForTx = true).id

    val lastBlock          = sender.blockAt(sender.height - 1)
    val blockById          = sender.blockById(ByteString.copyFrom(lastBlock.id.value().arr))
    val blocksSeq          = sender.blockSeq(1, 100)
    val blocksSeqByAddress = sender.blockSeqByAddress(lastBlock.header.generator.toAddress.toString, 1, 100)
    List(
      findTxInBlock(lastBlock, id),
      findTxInBlock(blockById, id),
      findTxInBlockSeq(blocksSeq, id),
      findTxInBlockSeq(blocksSeqByAddress, id)
    ).foreach(checkTx)

    /*
    val txFromInfoById  = sender.transactionInfo[TransactionInfo](id)
    val txFromByAddress = sender.transactionsByAddress(firstAddress, 100).find(_.id == id).get
    List(txFromInfoById, txFromByAddress).foreach(checkTxInfo(_, lastBlock.height))

    val stateChanges          = sender.debugStateChanges(id).stateChanges
    val stateChangesByAddress = sender.debugStateChangesByAddress(firstAddress, 1).flatMap(_.stateChanges).headOption
    List(stateChanges, stateChangesByAddress).foreach(checkStateChanges)
     */

    sender.getDataByKey(firstAddress, "check").head.value.boolValue.get shouldBe true
  }

  private def findTxInBlock(b: VanillaBlock, id: String): InvokeExpressionTransaction =
    findTxInBlockSeq(Seq(b), id)

  private def findTxInBlockSeq(b: Seq[VanillaBlock], id: String): InvokeExpressionTransaction =
    b.flatMap(_.transactionData).find(_.id.value().toString == id).get.asInstanceOf[InvokeExpressionTransaction]

  private def checkTx(tx: InvokeExpressionTransaction): Assertion = {
    tx.fee shouldBe invokeExpressionFee
    tx.feeAssetId shouldBe Waves
    tx.sender shouldBe firstAcc.publicKey
    tx.expression shouldBe expr
    tx.version shouldBe 1
    tx.timestamp should be > 0L
    tx.proofs.size shouldBe 1
    tx.chainId shouldBe AddressScheme.current.chainId
  }

  /*  private def checkTxInfo(tx: TransactionInfo, height: Int): Assertion = {
    tx.fee shouldBe invokeExpressionFee
    tx.sender.get shouldBe firstKeyPair.toAddress.toString
    tx.expression.get shouldBe expr.bytes.value().base64
    tx.version.get shouldBe 1
    tx.timestamp should be > 0L
    tx.chainId.get shouldBe AddressScheme.current.chainId
    tx.height shouldBe height
    checkStateChanges(tx.stateChanges)
  }*/

  private def checkStateChanges(s: Option[StateChangesDetails]): Assertion =
    s.get.data.head shouldBe PutDataResponse("boolean", true, "check")
}
