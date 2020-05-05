package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
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

  val scriptV3 =
    compile(
      s"""
         |{-# STDLIB_VERSION 3 #-}
         |{-# CONTENT_TYPE EXPRESSION #-}
         |
         | tx.bodyBytes.size() == 153530  # actually lower than ${Terms.DataTxMaxBytes}
         """.stripMargin
    )

  val scriptV4 =
    compile(
      s"""
         |{-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE EXPRESSION #-}
         |
         | tx.bodyBytes.size() == ${Terms.DataTxMaxProtoBytes}
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
    checkByteVectorLimit(firstAddress, maxDataEntriesV1, scriptV3, TxVersion.V1)
    checkByteVectorLimit(secondAddress, maxDataEntriesV2, scriptV4, TxVersion.V2)
  }

  private def checkByteVectorLimit(address: String, data: List[BinaryDataEntry], script: String, version: TxVersion) = {
    val setScriptId = sender.setScript(address, Some(script), setScriptFee, waitForTx = true).id
    sender.transactionInfo[TransactionInfo](setScriptId).script.get.startsWith("base64:") shouldBe true

    val scriptInfo = sender.addressScriptInfo(address)
    scriptInfo.script.isEmpty shouldBe false
    scriptInfo.scriptText.isEmpty shouldBe false
    scriptInfo.script.get.startsWith("base64:") shouldBe true

    sender.putData(address, data, version = version, fee = calcDataFee(data, version) + smartFee, waitForTx = true).id

    val increasedData = data.head.copy(value = data.head.value ++ ByteStr.fromBytes(1)) :: data.tail
    assertBadRequestAndMessage(
      sender.putData(address, increasedData, version = version, fee = calcDataFee(data, version) + smartFee),
      "Too big sequences requested"
    )
  }
}
