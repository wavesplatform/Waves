package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncGrpcApi.*
import com.wavesplatform.it.sync.{dataTxSupportedVersions, minFee}
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.protobuf.transaction.*
import com.wavesplatform.state.StringDataEntry
import com.wavesplatform.test.*
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.{DataTransaction, TxVersion}
import io.grpc.Status.Code

import scala.concurrent.duration.*

class DataTransactionGrpcSuite extends GrpcBaseTransactionSuite {

  val fourthAcc: KeyPair        = KeyPair("fourth_acc".getBytes("UTF-8"))
  val fourthAddress: ByteString = PBRecipients.create(Address.fromPublicKey(fourthAcc.publicKey)).getPublicKeyHash

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    sender.broadcastTransfer(firstAcc, Recipient().withPublicKeyHash(fourthAddress), 10.waves, minFee, waitForTx = true)
  }

  test("should not put 65-sized proof") {
    val keyPair = sender.generateKeyPair()
    sender.broadcastTransfer(sender.keyPair, PBRecipients.create(keyPair.toAddress), 1.waves, 100000L, waitForTx = true)
    sender.setScript(
      keyPair,
      Right(
        Some(
          ScriptCompiler
            .compile(
              """{-# STDLIB_VERSION 2 #-}
                |{-# CONTENT_TYPE EXPRESSION #-}
                |{-# SCRIPT_TYPE ACCOUNT #-}
                |
                |true""".stripMargin,
              ScriptEstimatorV1
            )
            .explicitGet()
            ._1
        )
      ),
      1000000L,
      waitForTx = true
    )
    val dataTx =
      PBTransactions.protobuf(
        DataTransaction.selfSigned(TxVersion.V1, keyPair, Seq(StringDataEntry("1", "test")), 700000L, System.currentTimeMillis()).explicitGet()
      )

    assertGrpcError(
      sender.broadcast(
        dataTx.getWavesTransaction,
        dataTx.proofs :+ ByteString.copyFrom(new Array[Byte](65)),
        waitForTx = true
      ),
      "Too large proof"
    )
  }

  test("sender's waves balance is decreased by fee.") {
    for (v <- dataTxSupportedVersions) {
      val firstBalance    = sender.wavesBalance(firstAddress).available
      val firstEffBalance = sender.wavesBalance(firstAddress).effective
      val entry           = DataEntry("int", DataEntry.Value.IntValue(0xcafebabe))
      val data            = List(entry)
      val fee             = calcDataFee(data, v)
      sender.putData(firstAcc, data, fee, version = v, waitForTx = true)
      sender.wavesBalance(firstAddress).available shouldBe firstBalance - fee
      sender.wavesBalance(firstAddress).effective shouldBe firstEffBalance - fee
    }
  }

  test("cannot put data without having enough waves") {
    for (v <- dataTxSupportedVersions) {
      val firstBalance    = sender.wavesBalance(firstAddress).available
      val firstEffBalance = sender.wavesBalance(firstAddress).effective
      val entry           = DataEntry("bool", DataEntry.Value.BoolValue(false))
      val data            = List(entry)

      assertGrpcError(sender.putData(firstAcc, data, firstBalance + 1, version = v), "Accounts balance errors", Code.INVALID_ARGUMENT)

      nodes.foreach(n => n.waitForHeight(n.height + 1))
      sender.wavesBalance(firstAddress).available shouldBe firstBalance
      sender.wavesBalance(firstAddress).effective shouldBe firstEffBalance
    }
  }

  test("cannot broadcast data transaction with invalid timestamp (more than allowed in future)") {
    val entry = DataEntry("bool", DataEntry.Value.BoolValue(false))
    val data  = List(entry)
    for (v <- dataTxSupportedVersions) {
      val firstBalance    = sender.wavesBalance(firstAddress).available
      val firstEffBalance = sender.wavesBalance(firstAddress).effective
      assertGrpcError(
        sender.putData(firstAcc, data, minFee, timestamp = System.currentTimeMillis() + 1.day.toMillis, version = v),
        "Transaction timestamp .* is more than .*ms in the future",
        Code.INVALID_ARGUMENT
      )
      sender.waitForHeight(sender.height + 1)
      sender.wavesBalance(firstAddress).available shouldBe firstBalance
      sender.wavesBalance(firstAddress).effective shouldBe firstEffBalance
    }
  }
  test("cannot broadcast data transaction with insufficient fee") {
    val entry = DataEntry("bool", DataEntry.Value.BoolValue(false))
    val data  = List(entry)
    for (v <- dataTxSupportedVersions) {
      val firstBalance    = sender.wavesBalance(firstAddress).available
      val firstEffBalance = sender.wavesBalance(firstAddress).effective
      assertGrpcError(sender.putData(firstAcc, data, minFee - 1), "Fee .* does not exceed minimal value", Code.INVALID_ARGUMENT)
      sender.waitForHeight(sender.height + 1)
      sender.wavesBalance(firstAddress).available shouldBe firstBalance
      sender.wavesBalance(firstAddress).effective shouldBe firstEffBalance
    }
  }

  test("max transaction size") {
    // Max size of transaction V1
    val maxKeySizeV1 = 100
    val key          = "\u6fae" * (maxKeySizeV1 - 1)
    val data         = List.tabulate(26)(n => DataEntry(key + n.toChar, DataEntry.Value.BinaryValue(ByteString.copyFrom(Array.fill(5599)(n.toByte)))))
    val fee          = calcDataFee(data, TxVersion.V1)
    sender.putData(firstAcc, data, fee, version = TxVersion.V1, waitForTx = true)

    // Max size of transaction V2
    val maxKeySizeV2 = 400
    val key2         = "u" * (maxKeySizeV2 - 1)
    val data2 =
      List.tabulate(5)(n => DataEntry(key2 + n.toChar, DataEntry.Value.BinaryValue(ByteString.copyFrom(Array.fill(Short.MaxValue)(n.toByte)))))
    val fee2 = calcDataFee(data2, TxVersion.V2)
    sender.putData(firstAcc, data2, fee2, version = TxVersion.V2, waitForTx = true)
  }

  test("data definition and retrieval") {
    for (v <- dataTxSupportedVersions) {
      val txSender        = if (v < 2) secondAcc else thirdAcc
      val txSenderAddress = if (v < 2) secondAddress else thirdAddress
      val intEntry        = DataEntry("int", DataEntry.Value.IntValue(8))
      val intList         = List(intEntry)
      sender.putData(txSender, intList, calcDataFee(intList, v), version = v, waitForTx = true)

      sender.getDataByKey(txSenderAddress, "int") shouldBe intList
      sender.getData(txSenderAddress) shouldBe intList

      val boolEntry = DataEntry("bool", DataEntry.Value.BoolValue(true))
      val boolList  = List(boolEntry)
      sender.putData(txSender, boolList, calcDataFee(boolList, v), version = v, waitForTx = true)

      val stringEntry = DataEntry("str", DataEntry.Value.StringValue("AAA"))
      val stringList  = List(stringEntry)
      sender.putData(txSender, stringList, calcDataFee(stringList, v), version = v, waitForTx = true)

      sender.getDataByKey(txSenderAddress, "int") shouldBe List(intEntry)
      sender.getDataByKey(txSenderAddress, "bool") shouldBe List(boolEntry)
      sender.getDataByKey(txSenderAddress, "str") shouldBe List(stringEntry)
      sender.getData(txSenderAddress) should contain theSameElementsAs boolList ++ intList ++ stringList

      // redefine int entry
      val reIntEntry = DataEntry("int", DataEntry.Value.IntValue(8))
      val reIntList  = List(intEntry)
      sender.putData(txSender, reIntList, calcDataFee(reIntList, v), version = v, waitForTx = true)

      sender.getDataByKey(txSenderAddress, "int") shouldBe List(reIntEntry)
      sender.getDataByKey(txSenderAddress, "bool") shouldBe List(boolEntry)
      sender.getDataByKey(txSenderAddress, "str") shouldBe List(stringEntry)
      sender.getData(txSenderAddress) should contain theSameElementsAs boolList ++ reIntList ++ stringList

      // define tx with all types
      val firstBalance       = sender.wavesBalance(txSenderAddress).available
      val firstEffBalance    = sender.wavesBalance(txSenderAddress).effective
      val intEntry2          = DataEntry("int", DataEntry.Value.IntValue(-127))
      val boolEntry2         = DataEntry("bool", DataEntry.Value.BoolValue(false))
      val blobEntry2         = DataEntry("blob", DataEntry.Value.BinaryValue(ByteString.copyFrom(Array[Byte](127.toByte, 0, 1, 1))))
      val stringEntry2       = DataEntry("str", DataEntry.Value.StringValue("BBBB"))
      val unicodeStringEntry = DataEntry("?&$#^123\\/.a:;'\"\r\n\t\u0000|%è&", DataEntry.Value.StringValue("specïal"))
      val dataAllTypes       = List(intEntry2, boolEntry2, blobEntry2, stringEntry2, unicodeStringEntry)
      val fee                = calcDataFee(dataAllTypes, v)
      sender.putData(txSender, dataAllTypes, fee, version = v, waitForTx = true)

      sender.getDataByKey(txSenderAddress, "int") shouldBe List(intEntry2)
      sender.getDataByKey(txSenderAddress, "bool") shouldBe List(boolEntry2)
      sender.getDataByKey(txSenderAddress, "blob") shouldBe List(blobEntry2)
      sender.getDataByKey(txSenderAddress, "str") shouldBe List(stringEntry2)
      sender.getData(txSenderAddress) should contain theSameElementsAs dataAllTypes

      sender.wavesBalance(txSenderAddress).available shouldBe firstBalance - fee
      sender.wavesBalance(txSenderAddress).effective shouldBe firstEffBalance - fee
    }
  }

  test("queries for nonexistent data") {
    sender.getDataByKey(firstAddress, "foo") shouldBe List.empty
    sender.getData(fourthAddress) shouldBe List.empty
  }

  test("update type for dataEntry") {
    for (v <- dataTxSupportedVersions) {
      val nonLatinKey = "\u05EA\u05E8\u05D1\u05D5\u05EA, \u05E1\u05E4\u05D5\u05E8\u05D8 \u05D5\u05EA\u05D9\u05D9\u05E8\u05D5\u05EA"
      val boolData    = List(DataEntry(nonLatinKey, DataEntry.Value.BoolValue(true)))
      val boolDataFee = calcDataFee(boolData, v)
      sender.putData(firstAcc, boolData, boolDataFee, version = v, waitForTx = true)
      sender.getDataByKey(firstAddress, nonLatinKey) shouldBe boolData

      val longData    = List(DataEntry(nonLatinKey, DataEntry.Value.IntValue(100500)))
      val longDataFee = calcDataFee(longData, v)
      sender.putData(firstAcc, longData, longDataFee, version = v, waitForTx = true)
      sender.getDataByKey(firstAddress, nonLatinKey) shouldBe longData
    }
  }

  test("try to send tx above limits of key, value and size") {
    for (v <- dataTxSupportedVersions) {
      val maxKeySize         = if (v < 2) 100 else 400
      val maxValueSize       = Short.MaxValue
      val maxEntryCount      = 100
      val tooBigKey          = "a" * (maxKeySize + 1)
      val tooBigKeyDataEntry = List(DataEntry(tooBigKey, DataEntry.Value.BoolValue(false)))

      assertGrpcError(
        sender.putData(firstAcc, tooBigKeyDataEntry, calcDataFee(tooBigKeyDataEntry, v), version = v),
        "Too big sequence requested",
        Code.INVALID_ARGUMENT
      )
      assertGrpcError(
        sender.putData(firstAcc, List(DataEntry("", DataEntry.Value.BoolValue(false))), 1.waves, version = v),
        "Empty key found",
        Code.INVALID_ARGUMENT
      )
      assertGrpcError(
        sender.putData(
          firstAcc,
          List(DataEntry("abc", DataEntry.Value.BoolValue(false)), DataEntry("abc", DataEntry.Value.BoolValue(false))),
          1.waves,
          version = v
        ),
        "Duplicated keys found",
        Code.INVALID_ARGUMENT
      )

      val extraValueData = List(DataEntry("key", DataEntry.Value.BinaryValue(ByteString.copyFrom(Array.fill(maxValueSize + 1)(1.toByte)))))
      assertGrpcError(
        sender.putData(firstAcc, extraValueData, calcDataFee(extraValueData, v), version = v),
        "Too big sequence requested",
        Code.INVALID_ARGUMENT
      )
      val largeBinData =
        List.tabulate(5)(n => DataEntry(tooBigKey + n.toString, DataEntry.Value.BinaryValue(ByteString.copyFrom(Array.fill(maxValueSize)(n.toByte)))))
      assertGrpcError(
        sender.putData(firstAcc, largeBinData, calcDataFee(largeBinData, v), version = v),
        "Too big sequence requested",
        Code.INVALID_ARGUMENT
      )

      val largeStrData = List.tabulate(5)(n => DataEntry(tooBigKey + n.toString, DataEntry.Value.StringValue("A" * maxValueSize)))
      assertGrpcError(
        sender.putData(firstAcc, largeStrData, calcDataFee(largeStrData, v), version = v),
        "Too big sequence requested",
        Code.INVALID_ARGUMENT
      )

      val tooManyEntriesData = List.tabulate(maxEntryCount + 1)(n => DataEntry(s"key$n", DataEntry.Value.IntValue(10)))
      assertGrpcError(
        sender.putData(firstAcc, tooManyEntriesData, calcDataFee(tooManyEntriesData, v), version = v),
        "Too big sequence requested",
        Code.INVALID_ARGUMENT
      )
    }
  }

  test("try to put empty data") {
    for (v <- dataTxSupportedVersions) {
      sender.putData(fourthAcc, List.empty, calcDataFee(List.empty, v))
      sender.getData(fourthAddress) shouldBe List.empty
    }
  }

  test("try to make address with 1000 DataEntries") {
    for (v <- dataTxSupportedVersions) {
      val dataSet = 0 until 200 flatMap (i =>
        List(
          DataEntry(s"int$i", DataEntry.Value.IntValue(1000 + i)),
          DataEntry(s"bool$i", DataEntry.Value.BoolValue(false)),
          DataEntry(s"blob$i", DataEntry.Value.BinaryValue(ByteString.copyFrom(Array[Byte](127.toByte, 0, 1, 1)))),
          DataEntry(s"str$i", DataEntry.Value.StringValue(s"hi there! + $i")),
          DataEntry(s"integer$i", DataEntry.Value.IntValue(1000 - i))
        )
      )

      val txIds = dataSet
        .grouped(100)
        .map(_.toList)
        .map(data =>
          PBTransactions.vanilla(sender.putData(fourthAcc, data, calcDataFee(data, v), version = v), unsafe = false).explicitGet().id().toString
        )
      txIds.foreach(tx => sender.waitForTransaction(tx))
      val r = scala.util.Random.nextInt(199)
      sender.getDataByKey(fourthAddress, s"int$r") shouldBe List(DataEntry(s"int$r", DataEntry.Value.IntValue(1000 + r)))
      sender.getDataByKey(fourthAddress, s"bool$r") shouldBe List(DataEntry(s"bool$r", DataEntry.Value.BoolValue(false)))
      sender.getDataByKey(fourthAddress, s"blob$r") shouldBe List(
        DataEntry(s"blob$r", DataEntry.Value.BinaryValue(ByteString.copyFrom(Array[Byte](127.toByte, 0, 1, 1))))
      )
      sender.getDataByKey(fourthAddress, s"str$r") shouldBe List(DataEntry(s"str$r", DataEntry.Value.StringValue(s"hi there! + $r")))
      sender.getDataByKey(fourthAddress, s"integer$r") shouldBe List(DataEntry(s"integer$r", DataEntry.Value.IntValue(1000 - r)))

      sender.getData(fourthAddress).size shouldBe 1000
    }
  }

  def calcDataFee(data: List[DataEntry], txVersion: Byte): Long = {
    if (txVersion < 2) {
      val dataSize = data.map(_.toByteArray.length).sum + 128
      if (dataSize > 1024) {
        minFee * (dataSize / 1024 + 1)
      } else minFee
    } else {
      val payload   = DataTransactionData(data).toByteArray
      val feeInUnit = 1 + (payload.length - 1) / 1024
      feeInUnit * 100000
    }
  }

}
