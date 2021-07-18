package com.wavesplatform.it.sync.smartcontract.freecall
import com.typesafe.config.Config
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.features.BlockchainFeatures.RideV6
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{Transaction, TransactionInfo}
import com.wavesplatform.it.sync.invokeExpressionFee
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.directives.values.StdLibVersion.V6
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.TestCompiler
import org.scalatest.CancelAfterFailure

class InvokeExpressionSuite extends BaseTransactionSuite with CancelAfterFailure {
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((RideV6.id, 0)))
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
    val id = sender.invokeExpression(firstKeyPair, expr)._1.id

    val txFromUnconfirmed = sender.utx().find(_.id == id).get
    check(txFromUnconfirmed, checkStatus = false)

    sender.waitForTransaction(id)
    val lastBlock = sender.lastBlock()

    val txFromLastBlock         = lastBlock.transactions.find(_.id == id).get
    val txFromBlockByHeight     = sender.blockAt(lastBlock.height).transactions.find(_.id == id).get
    val txFromBlockById         = sender.blockById(lastBlock.id).transactions.find(_.id == id).get
    val txFromBlockSeq          = sender.blockSeq(1, 100).flatMap(_.transactions).find(_.id == id).get
    val txFromBlockSeqByAddress = sender.blockSeqByAddress(sender.address, 1, 100).flatMap(_.transactions).find(_.id == id).get

    val txFromInfoById  = sender.transactionInfo[TransactionInfo](id)
    val txFromByAddress = sender.transactionsByAddress(firstAddress, 100).find(_.id == id).get

    List(txFromInfoById, txFromByAddress).foreach(checkInfo(_, lastBlock.height))
    List(txFromLastBlock, txFromBlockByHeight, txFromBlockById, txFromBlockSeq, txFromBlockSeqByAddress).foreach(check(_))

//    sender.debugStateChanges(id).stateChanges.get.data.head shouldBe PutDataResponse("boolean", true, "check")
//    sender.debugStateChangesByAddress(firstAddress, 1).flatMap(_.stateChanges.get.data).head shouldBe PutDataResponse("boolean", true, "check")
    sender.getDataByKey(firstAddress, "check").value shouldBe true
  }

  private def check(tx: Transaction, checkStatus: Boolean = true) = {
    tx.fee shouldBe invokeExpressionFee
    tx.feeAssetId shouldBe None
    tx.sender.get shouldBe firstKeyPair.toAddress.toString
    tx.senderPublicKey.get shouldBe firstKeyPair.publicKey.toString
    tx.expression.get shouldBe expr.bytes.value().base64
    tx.version.get shouldBe 1
    tx.timestamp should be > 0L
    tx.proofs.get.size shouldBe 1
    tx.chainId.get shouldBe AddressScheme.current.chainId
    if (checkStatus)
      tx.applicationStatus.get shouldBe "succeeded"
  }

  private def checkInfo(tx: TransactionInfo, height: Int) = {
    tx.fee shouldBe invokeExpressionFee
    tx.sender.get shouldBe firstKeyPair.toAddress.toString
    tx.expression.get shouldBe expr.bytes.value().base64
    tx.version.get shouldBe 1
    tx.timestamp should be > 0L
    tx.chainId.get shouldBe AddressScheme.current.chainId
    tx.height shouldBe height
  }
}
