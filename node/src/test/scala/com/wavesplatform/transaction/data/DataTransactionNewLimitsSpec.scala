package com.wavesplatform.transaction.data

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.network
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.test.FlatSpec
import com.wavesplatform.transaction.{DataTransaction, TxHelpers}

class DataTransactionNewLimitsSpec extends FlatSpec with WithDomain {
  def generateMaxAllowed(limit: Int): (BooleanDataEntry, IntegerDataEntry, StringDataEntry, Seq[BinaryDataEntry]) = {
    val KeySize = 1
    val ArbitraryEntriesCount = 10
    val bool    = BooleanDataEntry("1", value = true)
    val int     = IntegerDataEntry("2", Long.MaxValue)

    val bytesOrStrEntryLimit = (limit - KeySize * 2 - 8 - 1 - (KeySize * ArbitraryEntriesCount)) / ArbitraryEntriesCount
    val str                  = StringDataEntry("3", String.valueOf(Array.fill(bytesOrStrEntryLimit)('A')))
    val bytes                = (1 until ArbitraryEntriesCount).map(i => BinaryDataEntry((3 + i).toString, ByteStr(Array.fill(bytesOrStrEntryLimit)('A'.toByte))))
    (bool, int, str, bytes)
  }

  "Data transaction" should "handle new limits" in withDomain(DomainPresets.RideV6) { d =>
    d.helpers.creditWavesToDefaultSigner()

    val (bool, int, str, bytes) = generateMaxAllowed(DataTransaction.MaxBytes)
    val dataEntries             = Seq(bool, int, str) ++ bytes
    val dataTransaction         = TxHelpers.dataWithMultipleEntries(TxHelpers.defaultSigner, dataEntries)
    d.appendAndAssertSucceed(dataTransaction)
    
    val networkMessage = network.TransactionSpec.serializeData(dataTransaction)
    networkMessage.length shouldBe <= (network.TransactionSpec.maxLength)

    val protoNetworkMessage = network.PBTransactionSpec.serializeData(dataTransaction)
    protoNetworkMessage.length shouldBe <= (network.PBTransactionSpec.maxLength)
  }

  "Invoke transaction with data entries" should "handle new limits" in withDomain(DomainPresets.RideV6) { d =>
    val (bool, int, str, bytes) = generateMaxAllowed(ContractLimits.MaxWriteSetSizeInBytes)

    val script = TxHelpers.scriptV5(s"""
        |@Callable(i)
        |func default() = {
        |   [
        |     BooleanEntry("${bool.key}", ${bool.value}),
        |     IntegerEntry("${int.key}", ${int.value}),
        |     StringEntry("${str.key}", "${str.value}"),
        |     ${bytes.map(bytes => s"BinaryEntry(\"${bytes.key}\", base64'${bytes.value.base64Raw}')").mkString(",\n     ")}
        |   ]
        |}
        |""".stripMargin)

    val dAppAccount = TxHelpers.secondSigner
    d.helpers.creditWavesToDefaultSigner()
    d.helpers.creditWavesFromDefaultSigner(dAppAccount.toAddress)
    d.helpers.setScript(dAppAccount, script)
    d.appendAndAssertSucceed(TxHelpers.invoke(dAppAccount.toAddress, "default"))
  }
}
