package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.TransactionInfo
import com.wavesplatform.it.sync.{setScriptFee, _}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.state.{BinaryDataEntry, DataEntry}
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class DataTransactionBodyBytesByteVectorSuite extends BaseTransactionSuite {
  private def compile(scriptText: String) =
    ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1.bytes().base64

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .buildNonConflicting()

  private val maxDataTxV1bodyBytesSize = 153530
  // actually lower than Terms.DataTxMaxBytes

  private val scriptV3 =
    compile(
      s"""
         |{-# STDLIB_VERSION 3 #-}
         |{-# CONTENT_TYPE EXPRESSION #-}
         |
         | match tx {
         |    case dtx: DataTransaction =>
         |      dtx.bodyBytes.size() == $maxDataTxV1bodyBytesSize &&
         |      dtx.data.size() == 5
         |
         |   case _ =>
         |      throw("unexpected")
         | }
         |
       """.stripMargin
    )

  private val scriptV4 =
    compile(
      s"""
         |{-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE EXPRESSION #-}
         |
         | match tx {
         |   case dtx: DataTransaction =>
         |     dtx.bodyBytes.size() == ${Terms.DataTxMaxProtoBytes}         &&
         |     dtx.data.size() == 6                                         &&
         |     sigVerify(dtx.bodyBytes, dtx.proofs[0], dtx.senderPublicKey)
         |
         |  case _ =>
         |     throw("unexpected")
         | }
         |
       """.stripMargin
    )

  private val maxDataEntriesV1 =
    List(
      BinaryDataEntry("a", ByteStr.fill(22380)(1)),
      BinaryDataEntry("b", ByteStr.fill(DataEntry.MaxValueSize)(1)),
      BinaryDataEntry("c", ByteStr.fill(DataEntry.MaxValueSize)(1)),
      BinaryDataEntry("d", ByteStr.fill(DataEntry.MaxValueSize)(1)),
      BinaryDataEntry("e", ByteStr.fill(DataEntry.MaxValueSize)(1))
    )

  private val maxDataEntriesV2 =
    maxDataEntriesV1 :+ BinaryDataEntry("f", ByteStr.fill(12378)(1))

  test("filled data transaction body bytes") {
    checkByteVectorLimit(firstKeyPair, maxDataEntriesV1, scriptV3, TxVersion.V1)
    checkByteVectorLimit(secondKeyPair, maxDataEntriesV2, scriptV4, TxVersion.V2)
  }

  private def checkByteVectorLimit(address: KeyPair, data: List[BinaryDataEntry], script: String, version: TxVersion) = {
    val setScriptId = sender.setScript(address, Some(script), setScriptFee, waitForTx = true).id
    sender.transactionInfo[TransactionInfo](setScriptId).script.get.startsWith("base64:") shouldBe true

    val scriptInfo = sender.addressScriptInfo(address.toAddress.toString)
    scriptInfo.script.isEmpty shouldBe false
    scriptInfo.scriptText.isEmpty shouldBe false
    scriptInfo.script.get.startsWith("base64:") shouldBe true

    sender.putData(address, data, version = version, fee = calcDataFee(data, version) + smartFee, waitForTx = true).id

    val increasedData = data.head.copy(value = data.head.value ++ ByteStr.fromBytes(1)) :: data.tail
    assertBadRequestAndMessage(
      sender.putData(address, increasedData, version = version, fee = calcDataFee(data, version) + smartFee),
      "Too big sequence requested"
    )
  }
}
