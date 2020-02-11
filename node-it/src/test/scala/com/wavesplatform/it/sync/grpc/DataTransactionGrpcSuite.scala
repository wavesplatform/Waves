package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.minFee
import com.wavesplatform.it.util._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.protobuf.transaction.DataTransactionData.DataEntry
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.state.{DataEntry => VanillaDataEntry}
import com.wavesplatform.state.DataEntry.{MaxKeySize, MaxValueSize}
import com.wavesplatform.transaction.DataTransaction.MaxEntryCount
import com.wavesplatform.transaction.TxValidationError.TooBigArray
import io.grpc.Status.Code

import scala.concurrent.duration._

class DataTransactionGrpcSuite extends GrpcBaseTransactionSuite {
  test("sender's waves balance is decreased by fee.") {
    val firstBalance = sender.grpc.wavesBalance(firstAddress).available
    val firstEffBalance = sender.grpc.wavesBalance(firstAddress).effective
    val entry            = DataEntry("int", DataEntry.Value.IntValue(0xcafebabe))
    val data             = List(entry)
    val fee      = calcDataFee(data)
    sender.grpc.putData(firstAcc, data, fee, waitForTx = true)
    sender.grpc.wavesBalance(firstAddress).available shouldBe firstBalance - fee
    sender.grpc.wavesBalance(firstAddress).effective shouldBe firstEffBalance - fee
  }

  test("cannot put data without having enough waves") {
    val firstBalance = sender.grpc.wavesBalance(firstAddress).available
    val firstEffBalance = sender.grpc.wavesBalance(firstAddress).effective
    val entry = DataEntry("bool", DataEntry.Value.BoolValue(false))
    val data = List(entry)

    assertGrpcError(
      sender.grpc.putData(firstAcc, data, firstBalance + 1),
    "Accounts balance errors",
      Code.INVALID_ARGUMENT)

    nodes.foreach(n => n.grpc.waitForHeight(n.height + 1))
    sender.grpc.wavesBalance(firstAddress).available shouldBe firstBalance
    sender.grpc.wavesBalance(firstAddress).effective shouldBe firstEffBalance
  }

  test("invalid transaction should not be in UTX or blockchain") {
    val firstBalance = sender.grpc.wavesBalance(firstAddress).available
    val firstEffBalance = sender.grpc.wavesBalance(firstAddress).effective
    val entry = DataEntry("bool", DataEntry.Value.BoolValue(false))
    val data = List(entry)

    assertGrpcError(
      sender.grpc.putData(firstAcc, data, minFee, timestamp = System.currentTimeMillis() + 1.day.toMillis),
      "Transaction timestamp .* is more than .*ms in the future",
      Code.INVALID_ARGUMENT)
    assertGrpcError(
      sender.grpc.putData(firstAcc, data, minFee - 1),
      "Fee .* does not exceed minimal value",
      Code.INVALID_ARGUMENT)

    sender.grpc.waitForHeight(sender.height + 1)
    sender.grpc.wavesBalance(firstAddress).available shouldBe firstBalance
    sender.grpc.wavesBalance(firstAddress).effective shouldBe firstEffBalance
  }

  test("max transaction size") {
    val key  = "\u6fae" * (VanillaDataEntry.MaxKeySize - 1)
    val data = List.tabulate(26)(n => DataEntry(key + n.toChar, DataEntry.Value.BinaryValue(ByteString.copyFrom(Array.fill(5599)(n.toByte)))))
    val fee  = calcDataFee(data)

    sender.grpc.putData(firstAcc, data, fee, waitForTx = true)
  }

  test("data definition and retrieval") {
    val intEntry            = DataEntry("int", DataEntry.Value.IntValue(8))
    val intList             = List(intEntry)
    sender.grpc.putData(secondAcc, intList, calcDataFee(intList), waitForTx = true)

    sender.grpc.getDataByKey(secondAddress, "int") shouldBe intList
    sender.grpc.getData(secondAddress) shouldBe intList

    val boolEntry = DataEntry("bool", DataEntry.Value.BoolValue(true))
    val boolList  = List(boolEntry)
    sender.grpc.putData(secondAcc, boolList, calcDataFee(boolList), waitForTx = true)

    val stringEntry = DataEntry("str", DataEntry.Value.StringValue("AAA"))
    val stringList  = List(stringEntry)
    sender.grpc.putData(secondAcc, stringList, calcDataFee(stringList), waitForTx = true)

    sender.grpc.getDataByKey(secondAddress, "int") shouldBe List(intEntry)
    sender.grpc.getDataByKey(secondAddress, "bool") shouldBe List(boolEntry)
    sender.grpc.getDataByKey(secondAddress, "str") shouldBe List(stringEntry)
    sender.grpc.getData(secondAddress) should contain theSameElementsAs boolList ++ intList ++ stringList

    // redefine int entry
    val reIntEntry            = DataEntry("int", DataEntry.Value.IntValue(8))
    val reIntList             = List(intEntry)
    sender.grpc.putData(secondAcc, reIntList, calcDataFee(reIntList), waitForTx = true)

    sender.grpc.getDataByKey(secondAddress, "int") shouldBe List(reIntEntry)
    sender.grpc.getDataByKey(secondAddress, "bool") shouldBe List(boolEntry)
    sender.grpc.getDataByKey(secondAddress, "str") shouldBe List(stringEntry)
    sender.grpc.getData(secondAddress) should contain theSameElementsAs boolList ++ reIntList ++ stringList

    // define tx with all types
    val firstBalance = sender.grpc.wavesBalance(secondAddress).available
    val firstEffBalance = sender.grpc.wavesBalance(secondAddress).effective
    val intEntry2          = DataEntry("int", DataEntry.Value.IntValue(-127))
    val boolEntry2         = DataEntry("bool", DataEntry.Value.BoolValue(false))
    val blobEntry2         = DataEntry("blob", DataEntry.Value.BinaryValue(ByteString.copyFrom(Array[Byte](127.toByte, 0, 1, 1))))
    val stringEntry2       = DataEntry("str", DataEntry.Value.StringValue("BBBB"))
    val unicodeStringEntry = DataEntry("?&$#^123\\/.a:;'\"\r\n\t\u0000|%è&", DataEntry.Value.StringValue("specïal"))
    val dataAllTypes       = List(intEntry2, boolEntry2, blobEntry2, stringEntry2, unicodeStringEntry)
    val fee                = calcDataFee(dataAllTypes)
    sender.grpc.putData(secondAcc, dataAllTypes, fee, waitForTx = true)

    sender.grpc.getDataByKey(secondAddress, "int") shouldBe List(intEntry2)
    sender.grpc.getDataByKey(secondAddress, "bool") shouldBe List(boolEntry2)
    sender.grpc.getDataByKey(secondAddress, "blob") shouldBe List(blobEntry2)
    sender.grpc.getDataByKey(secondAddress, "str") shouldBe List(stringEntry2)
    sender.grpc.getData(secondAddress) should contain theSameElementsAs dataAllTypes

    sender.grpc.wavesBalance(secondAddress).available shouldBe firstBalance - fee
    sender.grpc.wavesBalance(secondAddress).effective shouldBe firstEffBalance - fee
  }

  test("queries for nonexistent data") {
    sender.grpc.getDataByKey(firstAddress, "foo") shouldBe List.empty
    sender.grpc.getData(thirdAddress) shouldBe List.empty
  }

  test("update type for dataEntry") {
    val nonLatinKey = "\u05EA\u05E8\u05D1\u05D5\u05EA, \u05E1\u05E4\u05D5\u05E8\u05D8 \u05D5\u05EA\u05D9\u05D9\u05E8\u05D5\u05EA"
    val boolData    = List(DataEntry(nonLatinKey, DataEntry.Value.BoolValue(true)))
    val boolDataFee = calcDataFee(boolData)
    sender.grpc.putData(firstAcc, boolData, boolDataFee, waitForTx = true)
    sender.grpc.getDataByKey(firstAddress, nonLatinKey) shouldBe boolData

    val longData    = List(DataEntry(nonLatinKey, DataEntry.Value.IntValue(100500)))
    val longDataFee = calcDataFee(longData)
    sender.grpc.putData(firstAcc, longData, longDataFee, waitForTx = true)
    sender.grpc.getDataByKey(firstAddress, nonLatinKey) shouldBe longData
  }

  test("try to send tx above limits of key, value and size") {
    val tooBigKey = "a" * (MaxKeySize + 1)
    val tooBigKeyDataEntry     = List(DataEntry(tooBigKey, DataEntry.Value.BoolValue(false)))

    assertGrpcError(sender.grpc.putData(firstAcc, tooBigKeyDataEntry, calcDataFee(tooBigKeyDataEntry)), s"$TooBigArray", Code.INVALID_ARGUMENT)
    assertGrpcError(sender.grpc.putData(firstAcc, List(DataEntry("", DataEntry.Value.BoolValue(false))), 1.waves), "Empty key found", Code.INVALID_ARGUMENT)
    assertGrpcError(
      sender.grpc.putData(firstAcc, List(DataEntry("abc", DataEntry.Value.BoolValue(false)), DataEntry("abc", DataEntry.Value.BoolValue(false))), 1.waves),
      "Duplicated keys found",
      Code.INVALID_ARGUMENT)

    val extraValueData = List(DataEntry("key", DataEntry.Value.BinaryValue(ByteString.copyFrom(Array.fill(MaxValueSize + 1)(1.toByte)))))
    assertGrpcError(sender.grpc.putData(firstAcc, extraValueData, calcDataFee(extraValueData)), s"$TooBigArray", Code.INVALID_ARGUMENT)
    val largeBinData = List.tabulate(5)(n => DataEntry(s"key$n", DataEntry.Value.BinaryValue(ByteString.copyFrom(Array.fill(MaxValueSize)(n.toByte)))))
    assertGrpcError(sender.grpc.putData(firstAcc, largeBinData, calcDataFee(largeBinData)), s"$TooBigArray", Code.INVALID_ARGUMENT)

    val largeStrData = List.tabulate(5)(n => DataEntry(s"key$n", DataEntry.Value.StringValue("A" * MaxValueSize)))
    assertGrpcError(sender.grpc.putData(firstAcc, largeStrData, calcDataFee(largeStrData)), s"$TooBigArray", Code.INVALID_ARGUMENT)

    val tooManyEntriesData = List.tabulate(MaxEntryCount + 1)(n => DataEntry(s"key$n", DataEntry.Value.IntValue(10)))
    assertGrpcError(sender.grpc.putData(firstAcc, tooManyEntriesData, calcDataFee(tooManyEntriesData)), s"$TooBigArray", Code.INVALID_ARGUMENT)
  }

  test("try to put empty data") {
    sender.grpc.putData(thirdAcc, List.empty, calcDataFee(List.empty))
    sender.grpc.getData(thirdAddress) shouldBe List.empty
  }

  test("try to make address with 1000 DataEntries") {
    val dataSet = 0 until 200 flatMap (
      i =>
        List(
          DataEntry(s"int$i", DataEntry.Value.IntValue(1000 + i)),
          DataEntry(s"bool$i", DataEntry.Value.BoolValue(false)),
          DataEntry(s"blob$i", DataEntry.Value.BinaryValue(ByteString.copyFrom(Array[Byte](127.toByte, 0, 1, 1)))),
          DataEntry(s"str$i", DataEntry.Value.StringValue(s"hi there! + $i")),
          DataEntry(s"integer$i", DataEntry.Value.IntValue(1000 - i)),
        )
      )

    val txIds = dataSet.grouped(100).map(_.toList).map(data => PBTransactions.vanilla(sender.grpc.putData(thirdAcc, data, calcDataFee(data))).explicitGet().id().toString)
    txIds.foreach(tx => sender.grpc.waitForTransaction(tx))
    val r = scala.util.Random.nextInt(199)
    sender.grpc.getDataByKey(thirdAddress, s"int$r") shouldBe List(DataEntry(s"int$r", DataEntry.Value.IntValue(1000 + r)))
    sender.grpc.getDataByKey(thirdAddress, s"bool$r") shouldBe List(DataEntry(s"bool$r", DataEntry.Value.BoolValue(false)))
    sender.grpc.getDataByKey(thirdAddress, s"blob$r") shouldBe List(DataEntry(s"blob$r", DataEntry.Value.BinaryValue(ByteString.copyFrom(Array[Byte](127.toByte, 0, 1, 1)))))
    sender.grpc.getDataByKey(thirdAddress, s"str$r") shouldBe List(DataEntry(s"str$r", DataEntry.Value.StringValue(s"hi there! + $r")))
    sender.grpc.getDataByKey(thirdAddress, s"integer$r") shouldBe List(DataEntry(s"integer$r", DataEntry.Value.IntValue(1000 - r)))

    sender.grpc.getData(thirdAddress).size shouldBe 1000
  }

  def calcDataFee(data: List[DataEntry]): Long = {
    val dataSize = data.map(_.toByteArray.length).sum + 128
    if (dataSize > 1024) {
      minFee * (dataSize / 1024 + 1)
    } else minFee
  }

}
