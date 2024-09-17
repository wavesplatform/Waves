package com.wavesplatform.transaction.data

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.network
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, DataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.test.{FlatSpec, NumericExt}
import com.wavesplatform.transaction.{DataTransaction, TxHelpers}

class DataTransactionNewLimitsSpec extends FlatSpec with WithDomain {
  def generateMaxAllowed(limit: Int, overLimit: Boolean = false): (BooleanDataEntry, IntegerDataEntry, StringDataEntry, Seq[BinaryDataEntry]) = {
    val KeySize               = 1
    val ArbitraryEntriesCount = 10
    val bool                  = BooleanDataEntry("1", value = true)
    val int                   = IntegerDataEntry("2", Long.MaxValue)

    val bytesOrStrEntryLimit = (limit - KeySize * 2 - 8 - 1 - (KeySize * ArbitraryEntriesCount)) / ArbitraryEntriesCount + (if (overLimit) 1 else 0)
    val str                  = StringDataEntry("3", String.valueOf(Array.fill(bytesOrStrEntryLimit)('A')))
    val bytes = (1 until ArbitraryEntriesCount).map(i => BinaryDataEntry((3 + i).toString, ByteStr(Array.fill(bytesOrStrEntryLimit)('A'.toByte))))
    (bool, int, str, bytes)
  }

  "Data transaction" should "handle new limits" in withDomain(DomainPresets.RideV6) { d =>
    d.helpers.creditWavesToDefaultSigner()

    val (bool, int, str, bytes) = generateMaxAllowed(DataTransaction.MaxRideV6Bytes)
    val dataEntries             = Seq(bool, int, str) ++ bytes
    val dataTransaction         = TxHelpers.dataV2(TxHelpers.defaultSigner, dataEntries, fee = 0.162.waves)
    d.appendAndAssertSucceed(dataTransaction)

    val networkMessage = network.TransactionSpec.serializeData(dataTransaction)
    networkMessage.length shouldBe <=(network.TransactionSpec.maxLength)

    val protoNetworkMessage = network.PBTransactionSpec.serializeData(dataTransaction)
    protoNetworkMessage.length shouldBe <=(network.PBTransactionSpec.maxLength)
  }

  it should "not handle over limits" in withDomain(DomainPresets.RideV6) { d =>
    d.helpers.creditWavesToDefaultSigner()

    val (bool, int, str, bytes) = generateMaxAllowed(DataTransaction.MaxRideV6Bytes, overLimit = true)
    val dataEntries             = Seq(bool, int, str) ++ bytes
    val dataTransaction         = TxHelpers.dataWithMultipleEntries(TxHelpers.defaultSigner, dataEntries)
    intercept[RuntimeException](d.appendBlock(dataTransaction))
  }

  it should "handle 400 bytes key" in withDomain(DomainPresets.RideV6) { d =>
    val dataEntry       = StringDataEntry("a" * 400, "test")
    val dataTransaction = TxHelpers.dataWithMultipleEntries(TxHelpers.defaultSigner, Seq(dataEntry))

    d.helpers.creditWavesToDefaultSigner()
    d.appendAndAssertSucceed(dataTransaction)
    d.blockchain.accountData(TxHelpers.defaultAddress, "a" * 400) shouldBe Some(dataEntry)
  }

  it should "not handle 401 bytes key" in withDomain(DomainPresets.RideV6) { d =>
    val dataEntry = StringDataEntry("a" * 401, "test")
    d.helpers.creditWavesToDefaultSigner()

    intercept[RuntimeException](d.appendBlock(TxHelpers.dataWithMultipleEntries(TxHelpers.defaultSigner, Seq(dataEntry)))).toString should include(
      "TooBigArray"
    )
    d.blockchain.accountData(TxHelpers.defaultAddress, "a" * 401) shouldBe None
  }

  it should "not handle 32768 bytes binary value" in withDomain(DomainPresets.RideV6) { d =>
    val dataEntry = BinaryDataEntry("test", ByteStr(("a" * 32768).getBytes("ASCII")))
    d.helpers.creditWavesToDefaultSigner()

    intercept[RuntimeException](d.appendBlock(TxHelpers.dataWithMultipleEntries(TxHelpers.defaultSigner, Seq(dataEntry)))).toString should include(
      "TooBigArray"
    )
    d.blockchain.accountData(TxHelpers.defaultAddress, "test") shouldBe None
  }

  it should "not handle 32768 bytes string value" in withDomain(DomainPresets.RideV6) { d =>
    val dataEntry = StringDataEntry("test", "a" * 32768)

    d.helpers.creditWavesToDefaultSigner()
    intercept[RuntimeException](d.appendBlock(TxHelpers.dataWithMultipleEntries(TxHelpers.defaultSigner, Seq(dataEntry)))).toString should include(
      "TooBigArray"
    )
    d.blockchain.accountData(TxHelpers.defaultAddress, "test") shouldBe None
  }

  it should "have lowered fee after V6" in {
    def calculateFee(preset: WavesSettings)(data: DataEntry[?]*): Long =
      withDomain(preset)(d => d.commonApi.calculateWavesFee(TxHelpers.dataWithMultipleEntries(TxHelpers.defaultSigner, data)))

    val bigData = (1 to 100).map(i => BinaryDataEntry(i.toString, ByteStr(new Array[Byte](1024 - i.toString.length))))
    calculateFee(DomainPresets.RideV5)(bigData*) shouldBe 0.101.waves
    calculateFee(DomainPresets.RideV6)(bigData*) shouldBe 0.100.waves
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
    d.appendAndAssertSucceed(TxHelpers.invoke(dAppAccount.toAddress, Some("default")))
  }
}
