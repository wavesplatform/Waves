package com.wavesplatform.api

import com.google.common.primitives.Ints
import com.google.protobuf.{ByteString, UnsafeByteOperations}
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.lang.API
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.protobuf.transaction.DataTransactionData

trait CommonGrpcConverters {
  def toByteString32(xs: Int*): ByteString = {
    require(xs.size < 4)
    UnsafeByteOperations.unsafeWrap(Array.concat(xs.map(Ints.toByteArray)*).padTo(32, 0.toByte))
  }

  def mkScript(scriptSrc: String): Script = {
    val estimator      = ScriptEstimatorV3(fixOverflow = true, overhead = false)
    val compiledScript = API.compile(input = scriptSrc, estimator).explicitGet()
    Script.fromBase64String(Base64.encode(compiledScript.bytes)).explicitGet()
  }

  def mkDataEntryUpdate(address: Address, key: String, valueBefore: Option[Long], valueAfter: Option[Long]): StateUpdate.DataEntryUpdate =
    StateUpdate.DataEntryUpdate(
      address = DefaultBlockchainApi.toPb(address),
      dataEntryBefore = valueBefore.map { valueBefore =>
        DataTransactionData.DataEntry(key, DataTransactionData.DataEntry.Value.IntValue(valueBefore))
      },
      dataEntry = valueAfter.map { valueAfter => DataTransactionData.DataEntry(key, DataTransactionData.DataEntry.Value.IntValue(valueAfter)) }
    )

}

object CommonGrpcConverters extends CommonGrpcConverters
