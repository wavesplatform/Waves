package com.wavesplatform.it.sync.smartcontract.freecall
import com.typesafe.config.Config
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.api.http.ApiError.StateCheckFailed
import com.wavesplatform.features.BlockchainFeatures.ContinuationTransaction
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.{PutDataResponse, StateChangesDetails, Transaction, TransactionInfo}
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.sync.invokeExpressionFee
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.transaction.smart.InvokeExpressionTransaction
import org.scalatest.{Assertion, CancelAfterFailure}

class InvokeExpressionSuite extends BaseTransactionSuite with CancelAfterFailure {
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((ContinuationTransaction.id, 0)))
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
    val id        = sender.invokeExpression(firstKeyPair, expr, waitForTx = true)._1.id
    val lastBlock = sender.lastBlock()

    val txFromLastBlock         = lastBlock.transactions.find(_.id == id).get
    val txFromBlockByHeight     = sender.blockAt(lastBlock.height).transactions.find(_.id == id).get
    val txFromBlockById         = sender.blockById(lastBlock.id).transactions.find(_.id == id).get
    val txFromBlockSeq          = sender.blockSeq(1, 100).flatMap(_.transactions).find(_.id == id).get
    val txFromBlockSeqByAddress = sender.blockSeqByAddress(sender.address, 1, 100).flatMap(_.transactions).find(_.id == id).get
    List(txFromLastBlock, txFromBlockByHeight, txFromBlockById, txFromBlockSeq, txFromBlockSeqByAddress).foreach(checkTx(_))

    val txFromInfoById  = sender.transactionInfo[TransactionInfo](id)
    val txFromByAddress = sender.transactionsByAddress(firstAddress, 100).find(_.id == id).get
    List(txFromInfoById, txFromByAddress).foreach(checkTxInfo(_, lastBlock.height))

    val stateChanges          = sender.stateChanges(id).stateChanges
    val stateChangesByAddress = sender.transactionsByAddress(firstAddress, 1).flatMap(_.stateChanges).headOption
    List(stateChanges, stateChangesByAddress).foreach(checkStateChanges)

    sender.getDataByKey(firstAddress, "check").value shouldBe true
  }

  test("reject on illegal fields") {
    val unsupportedVersion = 4
    assertApiError(
      sender.invokeExpression(firstKeyPair, expr, version = unsupportedVersion.toByte),
      AssertiveApiError(StateCheckFailed.Id, s"Transaction version $unsupportedVersion has not been activated yet", matchMessage = true)
    )
  }

  private def checkTx(tx: Transaction, checkStatus: Boolean = true): Assertion = {
    if (checkStatus)
      tx.applicationStatus.get shouldBe "succeeded"
    tx.fee shouldBe invokeExpressionFee
    tx.feeAssetId shouldBe None
    tx.sender.get shouldBe firstKeyPair.toAddress.toString
    tx.senderPublicKey.get shouldBe firstKeyPair.publicKey.toString
    tx.expression.get shouldBe expr.bytes.value().base64
    tx.version.get shouldBe 1
    tx.timestamp should be > 0L
    tx.proofs.get.size shouldBe 1
    tx.chainId.get shouldBe AddressScheme.current.chainId
  }

  private def checkTxInfo(tx: TransactionInfo, height: Int): Assertion = {
    tx.fee shouldBe invokeExpressionFee
    tx.sender.get shouldBe firstKeyPair.toAddress.toString
    tx.expression.get shouldBe expr.bytes.value().base64
    tx.version.get shouldBe 1
    tx.timestamp should be > 0L
    tx.chainId.get shouldBe AddressScheme.current.chainId
    tx.height shouldBe height
    checkStateChanges(tx.stateChanges)
  }

  private def checkStateChanges(s: Option[StateChangesDetails]): Assertion =
    s.get.data.head shouldBe PutDataResponse("boolean", true, "check")
}
