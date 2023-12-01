package com.wavesplatform.it.sync.transactions

import com.google.common.primitives.Ints
import com.typesafe.config.Config
import com.wavesplatform.account.{AddressScheme, KeyPair}
import com.wavesplatform.api.http.ApiError.{CustomValidationError, TooBigArrayAllocation, WrongJson}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.api.{TransactionInfo, UnexpectedStatusCodeException}
import com.wavesplatform.it.sync.{calcDataFee, minFee, *}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.test.*
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, DataEntry, EmptyDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.{DataTransaction, Proofs, TxVersion}
import org.scalatest.{Assertion, Assertions, EitherValues}
import play.api.libs.json.*

import scala.concurrent.duration.*
import scala.util.{Failure, Random, Try}

class DataTransactionSuite extends BaseTransactionSuite with EitherValues {
  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.raw("waves.blockchain.custom.functionality.blocks-for-feature-activation = 1"))
      .overrideBase(_.raw("waves.blockchain.custom.functionality.feature-check-blocks-period = 1"))
      .overrideBase(_.preactivatedFeatures(15 -> 0))
      .withDefault(1)
      .withSpecial(1, _.nonMiner)
      .buildNonConflicting()

  private lazy val fourthKeyPair         = sender.createKeyPair()
  private lazy val fourthAddress: String = fourthKeyPair.toAddress.toString

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    // explicitly create two new addresses in node's wallet
    sender.postForm("/addresses")
    sender.postForm("/addresses")
    sender.transfer(firstKeyPair, fourthAddress, 10.waves, minFee, waitForTx = true)
  }

  test("should not put 65-sized proof") {
    val keyPair = sender.createKeyPair()
    sender.transfer(sender.keyPair, keyPair.toAddress.toString, 1.waves, waitForTx = true)
    sender.setScript(
      keyPair,
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
          .bytes()
          .base64
      ),
      waitForTx = true
    )
    val dataTx =
      DataTransaction.selfSigned(TxVersion.V1, keyPair, Seq(StringDataEntry("1", "test")), 700000L, System.currentTimeMillis()).explicitGet()

    val brokenProofs = dataTx.copy(proofs = Proofs(dataTx.proofs.proofs :+ ByteStr(new Array[Byte](65))))
    assertBadRequestAndResponse(sender.signedBroadcast(brokenProofs.json(), waitForTx = true), "Too large proof")
  }

  test("put and remove keys") {
    def dataEntries(i: Int): List[DataEntry[_]] =
      List(
        BooleanDataEntry(s"bool-key-$i", i % 2 == 0),
        IntegerDataEntry(s"int-key-$i", i),
        BinaryDataEntry(s"binary-key-$i", ByteStr(Ints.toByteArray(i))),
        StringDataEntry(s"string-key-$i", s"$i-value")
      )

    def updateDataEntry(de: DataEntry[_]): DataEntry[_] = de match {
      case BooleanDataEntry(k, v) => BooleanDataEntry(k, !v)
      case IntegerDataEntry(k, v) => IntegerDataEntry(k, v + 100)
      case BinaryDataEntry(k, v)  => BinaryDataEntry(k, ByteStr(v.arr :+ v.arr.length.toByte))
      case StringDataEntry(k, v)  => StringDataEntry(k, v.reverse)
      case e                      => e
    }

    // can put data
    val putDataEntries = (1 to 25).flatMap(i => dataEntries(i)).toList
    val putTxId        = sender.putData(sender.keyPair, putDataEntries, calcDataFee(putDataEntries, TxVersion.V1)).id
    nodes.waitForTransaction(putTxId)

    // can put new, update and remove existed in the same transaction
    val updatedDatEntries = putDataEntries.take(25).map(updateDataEntry)
    val newDataEntries    = (26 to 30).flatMap(i => dataEntries(i))
    val updateAndRemoveDataEntries =
      updatedDatEntries ++                                                // 25 keys to update
        newDataEntries ++                                                 // 20 new keys
        putDataEntries.takeRight(25).map(kv => EmptyDataEntry(kv.key)) ++ // 25 keys to remove
        (1 to 25).map(k => EmptyDataEntry(s"unknown-$k"))                 // 20 unknown keys to remove

    val updateAndRemoveTxId =
      sender.broadcastData(sender.keyPair, updateAndRemoveDataEntries, calcDataFee(updateAndRemoveDataEntries, TxVersion.V2)).id

    nodes.waitForTransaction(updateAndRemoveTxId)

    sender.getData(sender.address) should contain theSameElementsAs updatedDatEntries ++ putDataEntries.slice(25, 75) ++ newDataEntries

    // can reuse removed keys
    val reusedData = putDataEntries.takeRight(25).map(updateDataEntry)
    val reuseTxId =
      sender.broadcastData(sender.keyPair, reusedData, calcDataFee(reusedData, TxVersion.V1), version = TxVersion.V1).id

    nodes.waitForTransaction(reuseTxId)

    sender.getData(sender.address) should contain theSameElementsAs updatedDatEntries ++ putDataEntries.slice(25, 75) ++ reusedData ++ newDataEntries

    // can't update and remove keys in the same transaction
    val sameKeyEntries = updateAndRemoveDataEntries.tail :+ EmptyDataEntry(updateAndRemoveDataEntries(1).key)
    assertApiError(
      sender.broadcastData(sender.keyPair, sameKeyEntries, calcDataFee(sameKeyEntries, TxVersion.V2), version = TxVersion.V2),
      CustomValidationError("Duplicated keys found")
    )

    // able to "remove" nonexistent key (account state won't be changed, but transaction should be succesfully broadcasted)
    sender.broadcastData(
      sender.keyPair,
      List(EmptyDataEntry("nonexistentkey")),
      calcDataFee(List(EmptyDataEntry("nonexistentkey")), TxVersion.V2),
      waitForTx = true
    )
    sender.getData(sender.address).filter(_.key == "nonexistentkey") shouldBe List.empty

    // max number of data entries is 100
    val tooLargeSizeDataEntries = updateAndRemoveDataEntries ++ (1 to 11).map(k => EmptyDataEntry(s"another-unknown-$k"))
    assertApiError(
      sender.broadcastData(sender.keyPair, tooLargeSizeDataEntries, calcDataFee(tooLargeSizeDataEntries, TxVersion.V2), version = TxVersion.V2),
      TooBigArrayAllocation
    )

    // max key size is 400 byte
    val tooLargeKeyDataEntries = List(BinaryDataEntry("a" * 401, ByteStr("value".getBytes("utf-8"))))
    assertApiError(
      sender.broadcastData(sender.keyPair, tooLargeKeyDataEntries, calcDataFee(tooLargeKeyDataEntries, TxVersion.V2), version = TxVersion.V2),
      TooBigArrayAllocation
    )

    // can put and remove same data within one block
    nodes.waitForHeightArise()
    val putDataEntries2 = List(IntegerDataEntry("del", 42))
    val putDataTxId     = sender.putData(sender.keyPair, putDataEntries2, calcDataFee(putDataEntries2, TxVersion.V1) * 10).id
    val removeDataTxId  = sender.broadcastData(sender.keyPair, List(EmptyDataEntry("del")), calcDataFee(List(EmptyDataEntry("del")), TxVersion.V2)).id
    nodes.waitForTransaction(putDataTxId)
    nodes.waitForTransaction(removeDataTxId)
    sender.getData(sender.address).filter(_.key == "del") shouldBe List.empty
  }

  test("sender's waves balance is decreased by fee") {
    for (v <- dataTxSupportedVersions) {
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val entry            = IntegerDataEntry("int", 0xcafebabe)
      val data             = List(entry)
      val dataFee          = calcDataFee(data, v)
      val dataTx           = sender.putData(firstKeyPair, data, version = v, fee = dataFee)
      nodes.waitForTransaction(dataTx.id)
      if (v > 2) {
        dataTx.chainId shouldBe Some(AddressScheme.current.chainId)
        sender.transactionInfo[TransactionInfo](dataTx.id).chainId shouldBe Some(AddressScheme.current.chainId)
      }
      miner.assertBalances(firstAddress, balance1 - dataFee, eff1 - dataFee)
    }
  }

  test("cannot broadcast data without having enough waves") {
    for (v <- dataTxSupportedVersions) {
      val (balance1, eff1) = miner.accountBalances(firstAddress)

      val data = List(BooleanDataEntry("bool", false))
      assertBadRequestAndResponse(sender.putData(firstKeyPair, data, balance1 + 1, version = v), "Accounts balance errors")
      miner.assertBalances(firstAddress, balance1, eff1)

      val leaseAmount = 1.waves
      val leaseId     = sender.lease(firstKeyPair, secondAddress, leaseAmount, minFee).id
      nodes.waitForTransaction(leaseId)

      assertBadRequestAndResponse(sender.putData(firstKeyPair, data, balance1 - leaseAmount, version = v), "Accounts balance errors")
      miner.assertBalances(firstAddress, balance1 - minFee, eff1 - leaseAmount - minFee)
    }
  }

  test("cannot broadcast data transaction with invalid timestamp (more than allowed in future)") {
    val dataEntry = List(IntegerDataEntry("int", 177))
    for (v <- dataTxSupportedVersions) {
      val (balance1, eff1)        = miner.accountBalances(firstAddress)
      val invalidDataTxFromFuture = data(entries = dataEntry, timestamp = System.currentTimeMillis + 1.day.toMillis, version = v)
      assertBadRequestAndResponse(
        sender.broadcastRequest(invalidDataTxFromFuture.json()),
        "Transaction timestamp .* is more than .*ms in the future"
      )
      nodes.foreach(_.ensureTxDoesntExist(invalidDataTxFromFuture.id().toString))
      nodes.waitForHeightArise()
      miner.assertBalances(firstAddress, balance1, eff1)
    }
  }

  test("cannot broadcast data transaction with insufficient fee") {
    val dataEntry = List(IntegerDataEntry("int", 177))
    for (v <- dataTxSupportedVersions) {
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val invalidDataTx    = data(entries = dataEntry, fee = calcDataFee(dataEntry, v) - 1, version = v)
      assertBadRequestAndResponse(
        sender.broadcastRequest(invalidDataTx.json()),
        "Fee .* does not exceed minimal value"
      )
      nodes.foreach(_.ensureTxDoesntExist(invalidDataTx.id().toString))
      nodes.waitForHeightArise()
      miner.assertBalances(firstAddress, balance1, eff1)
    }
  }

  test("max transaction size") {
    // Max size of transaction V1
    val maxKeySizeV1 = 100
    val key          = "\u6fae" * (maxKeySizeV1 - 1)
    val data         = List.tabulate(26)(n => BinaryDataEntry(key + n.toChar, ByteStr(Array.fill(5599)(n.toByte))))
    val fee          = calcDataFee(data, TxVersion.V1)
    val txId         = sender.putData(firstKeyPair, data, fee, version = TxVersion.V1).id
    nodes.waitForTransaction(txId)

    // Max size of transaction V2
    val maxKeySizeV2 = 400
    val key2         = "u" * (maxKeySizeV2 - 1)
    val data2        = List.tabulate(5)(n => BinaryDataEntry(key2 + n.toChar, ByteStr(Array.fill(Short.MaxValue)(n.toByte))))
    val fee2         = calcDataFee(data2, TxVersion.V2)
    val txId2        = sender.putData(firstKeyPair, data2, fee2, version = TxVersion.V2).id
    nodes.waitForTransaction(txId2)

  }

  test("data definition and retrieval") {
    for (v <- dataTxSupportedVersions) {
      val txSender = if (v < 2) secondKeyPair else thirdKeyPair
      // define first int entry
      val intEntry = IntegerDataEntry("int", 8)
      val intList  = List(intEntry)
      val tx1      = sender.putData(txSender, intList, calcDataFee(intList, v), version = v).id
      nodes.waitForTransaction(tx1)

      val txSenderAddress = txSender.toAddress.toString
      sender.getDataByKey(txSenderAddress, "int") shouldBe intEntry
      sender.getData(txSenderAddress) shouldBe intList

      // define boolean entry
      val boolEntry = BooleanDataEntry("bool", true)
      val boolList  = List(boolEntry)
      val tx2       = sender.putData(txSender, boolList, calcDataFee(boolList, v), version = v).id
      nodes.waitForTransaction(tx2)

      // define string entry
      val stringEntry = StringDataEntry("str", "AAA")
      val stringList  = List(stringEntry)
      val txS         = sender.putData(txSender, stringList, calcDataFee(stringList, v), version = v).id
      nodes.waitForTransaction(txS)

      sender.getDataByKey(txSenderAddress, "int") shouldBe intEntry
      sender.getDataByKey(txSenderAddress, "bool") shouldBe boolEntry
      sender.getDataByKey(txSenderAddress, "str") shouldBe stringEntry
      sender.getData(txSenderAddress) shouldBe boolList ++ intList ++ stringList

      // redefine int entry
      val reIntEntry = IntegerDataEntry("int", 10)
      val reIntList  = List(reIntEntry)
      val tx3        = sender.putData(txSender, reIntList, calcDataFee(reIntList, v), version = v).id
      nodes.waitForTransaction(tx3)

      sender.getDataByKey(txSenderAddress, "int") shouldBe reIntEntry
      sender.getDataByKey(txSenderAddress, "bool") shouldBe boolEntry
      sender.getData(txSenderAddress) shouldBe boolList ++ reIntList ++ stringList

      // define tx with all types
      val (balance2, eff2)   = miner.accountBalances(txSenderAddress)
      val intEntry2          = IntegerDataEntry("int", -127)
      val boolEntry2         = BooleanDataEntry("bool", false)
      val blobEntry2         = BinaryDataEntry("blob", ByteStr(Array[Byte](127.toByte, 0, 1, 1)))
      val stringEntry2       = StringDataEntry("str", "BBBB")
      val unicodeStringEntry = StringDataEntry("?&$#^123\\/.a:;'\"\r\n\t\u0000|%è&", "specïal")
      val dataAllTypes       = List(intEntry2, boolEntry2, blobEntry2, stringEntry2, unicodeStringEntry)
      val fee                = calcDataFee(dataAllTypes, v)
      val txId               = sender.putData(txSender, dataAllTypes, fee, version = v).id
      nodes.waitForTransaction(txId)

      sender.getDataByKey(txSenderAddress, "int") shouldBe intEntry2
      sender.getDataByKey(txSenderAddress, "bool") shouldBe boolEntry2
      sender.getDataByKey(txSenderAddress, "blob") shouldBe blobEntry2
      sender.getDataByKey(txSenderAddress, "str") shouldBe stringEntry2
      sender.getData(txSenderAddress) shouldBe dataAllTypes.sortBy(_.key)

      miner.assertBalances(txSenderAddress, balance2 - fee, eff2 - fee)

      val json = Json.parse(sender.get(s"/transactions/info/$txId").getResponseBody)
      ((json \ "data")(2) \ "value").as[String].startsWith("base64:") shouldBe true
    }
  }

  test("queries for multiple keys") {
    val tooBigKey = "toobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkey"
    val keys      = Seq("int", "bool", "int", "blob", "?&$#^123\\/.a:;'\"\r\n\t\u0000|%è&", "str", "inexisted_key", tooBigKey)
    val values    = Seq[Any](-127, false, -127, ByteStr(Array[Byte](127.toByte, 0, 1, 1)), "specïal", "BBBB")

    val list     = sender.getDataList(secondAddress, keys*).map(_.value)
    val jsonList = sender.getDataListJson(secondAddress, keys*).map(_.value)
    val postList = sender.getDataListPost(secondAddress, keys*).map(_.value)

    list shouldBe values
    jsonList shouldBe list
    postList shouldBe list
  }

  test("queries for nonexistent data") {
    def assertNotFound(url: String): Assertion = Try(sender.get(url)) match {
      case Failure(ApiCallException(UnexpectedStatusCodeException(_, _, statusCode, responseBody))) =>
        statusCode shouldBe 404
        responseBody should include("no data for this key")
      case _ => Assertions.fail("Expected 404")
    }

    assertNotFound(s"/addresses/data/$secondAddress/foo")
    assertNotFound(s"/addresses/data/$thirdAddress/foo")
    sender.getData(fourthAddress) shouldBe List.empty
  }

  test("update type for dataEntry") {
    for (v <- dataTxSupportedVersions) {
      val nonLatinKey = "\u05EA\u05E8\u05D1\u05D5\u05EA, \u05E1\u05E4\u05D5\u05E8\u05D8 \u05D5\u05EA\u05D9\u05D9\u05E8\u05D5\u05EA"
      val boolData    = List(BooleanDataEntry(nonLatinKey, true))
      val boolDataFee = calcDataFee(boolData, v)
      val firstTx     = sender.putData(firstKeyPair, boolData, boolDataFee, version = v).id
      nodes.waitForTransaction(firstTx)
      sender.getDataByKey(firstAddress, nonLatinKey) shouldBe boolData.head

      val longData    = List(IntegerDataEntry(nonLatinKey, 100500))
      val longDataFee = calcDataFee(longData, v)
      val secondTx    = sender.putData(firstKeyPair, longData, longDataFee, version = v).id
      nodes.waitForTransaction(secondTx)
      sender.getDataByKey(firstAddress, nonLatinKey) shouldBe longData.head
    }
  }

  test("transaction requires a valid proof") {
    for (v <- dataTxSupportedVersions) {
      def request: JsObject =
        DataTransaction
          .selfSigned(
            v,
            firstKeyPair,
            List(IntegerDataEntry("int", 333)),
            minFee,
            System.currentTimeMillis()
          )
          .explicitGet()
          .json()

      def id(obj: JsObject): String = obj.value("id").as[String]

      val noProof = request - "proofs"
      assertBadRequestAndResponse(sender.postJson("/transactions/broadcast", noProof), s"${WrongJson.WrongJsonDataMessage}.*proofs.*missing")
      nodes.foreach(_.ensureTxDoesntExist(id(noProof)))

      val badProof = request ++ Json.obj("proofs" -> Seq(Base58.encode(Array.fill(64)(Random.nextInt().toByte))))
      assertBadRequestAndResponse(sender.postJson("/transactions/broadcast", badProof), "Proof doesn't validate as signature")
      nodes.foreach(_.ensureTxDoesntExist(id(badProof)))

      val withProof = request
      assert((withProof \ "proofs").as[Seq[String]].lengthCompare(1) == 0)
      sender.postJson("/transactions/broadcast", withProof)
      nodes.waitForTransaction(id(withProof))
    }
  }

  private def postDataTxJson(source: KeyPair, data: Seq[DataEntry[_]], fee: Long, version: Byte) =
    sender.signedBroadcast(
      Json.obj(
        "type"            -> DataTransaction.typeId,
        "sender"          -> source.toAddress,
        "senderPublicKey" -> source.publicKey,
        "fee"             -> fee,
        "version"         -> version,
        "data"            -> data,
        "proofs"          -> Json.arr(JsString("")),
        "timestamp"       -> System.currentTimeMillis()
      )
    )

  test("try to send tx above limits of key, value and entries count") {
    for (v <- dataTxSupportedVersions) {
      val maxKeySize    = 400
      val maxValueSize  = Short.MaxValue
      val maxEntryCount = 100
      val TooBig        = "Too big sequence requested"
      val extraKey      = "a" * (maxKeySize + 1)
      val data          = List(BooleanDataEntry(extraKey, false))

      assertBadRequestAndResponse(sender.putData(firstKeyPair, data, calcDataFee(data, TxVersion.V1), version = v), TooBig)
      assertBadRequestAndResponse(sender.putData(firstKeyPair, List(IntegerDataEntry("", 4)), 100000, version = v), "Empty key found")
      assertBadRequestAndResponse(
        sender.putData(firstKeyPair, List(IntegerDataEntry("abc", 4), IntegerDataEntry("abc", 5)), 100000, version = v),
        "Duplicated keys found"
      )

      val extraValueData = List(BinaryDataEntry("key", ByteStr(Array.fill(maxValueSize + 1)(1.toByte))))
      assertBadRequestAndResponse(postDataTxJson(firstKeyPair, extraValueData, 1.waves, version = v), TooBig)

      val largeBinData = List.tabulate(5)(n => BinaryDataEntry(extraKey + n.toString, ByteStr(Array.fill(maxValueSize)(n.toByte))))
      assertBadRequestAndResponse(postDataTxJson(firstKeyPair, largeBinData, 1.waves, version = v), TooBig)

      val largeStrData = List.tabulate(5)(n => StringDataEntry(extraKey + n.toString, "A" * maxValueSize))
      assertBadRequestAndResponse(postDataTxJson(firstKeyPair, largeStrData, 1.waves, version = v), TooBig)

      val tooManyEntriesData = List.tabulate(maxEntryCount + 1)(n => IntegerDataEntry("key" + n.toString, 88))
      assertBadRequestAndResponse(postDataTxJson(firstKeyPair, tooManyEntriesData, 1.waves, version = v), TooBig)
    }
  }

  test("try to put empty data") {
    for (v <- dataTxSupportedVersions) {
      val noDataTx = sender.putData(fourthKeyPair, List.empty, calcDataFee(List.empty, v), version = v).id
      nodes.waitForTransaction(noDataTx)
      sender.getData(fourthAddress) shouldBe List.empty
    }
  }

  test("try to make address with 1000 DataEntries") {
    for (v <- dataTxSupportedVersions) {
      val dataSet = 0 until 200 flatMap (i =>
        List(
          IntegerDataEntry(s"int$i", 1000 + i),
          BooleanDataEntry(s"bool$i", false),
          BinaryDataEntry(s"blob$i", ByteStr(Array[Byte](127.toByte, 0, 1, 1))),
          StringDataEntry(s"str$i", s"hi there! + $i"),
          IntegerDataEntry(s"integer$i", 1000 - i)
        )
      )

      val txIds = dataSet.grouped(100).map(_.toList).map(data => sender.putData(fourthKeyPair, data, calcDataFee(data, v), version = v).id)
      txIds foreach nodes.waitForTransaction

      val r = scala.util.Random.nextInt(199)
      sender.getDataByKey(fourthAddress, s"int$r") shouldBe IntegerDataEntry(s"int$r", 1000 + r)
      sender.getDataByKey(fourthAddress, s"bool$r") shouldBe BooleanDataEntry(s"bool$r", false)
      sender.getDataByKey(fourthAddress, s"blob$r") shouldBe BinaryDataEntry(s"blob$r", ByteStr(Array[Byte](127.toByte, 0, 1, 1)))
      sender.getDataByKey(fourthAddress, s"str$r") shouldBe StringDataEntry(s"str$r", s"hi there! + $r")
      sender.getDataByKey(fourthAddress, s"integer$r") shouldBe IntegerDataEntry(s"integer$r", 1000 - r)

      sender.getData(fourthAddress).size shouldBe 1000
    }
  }

  test("put data in liquid block") {
    val newAddress = sender.createKeyPair()
    val entries    = List(StringDataEntry("test", "test"))
    sender.transfer(firstKeyPair, newAddress.toAddress.toString, 2 waves, 1 waves, waitForTx = true)
    sender.broadcastData(newAddress, entries, 0.1 waves, waitForTx = true)
    sender.getData(newAddress.toAddress.toString) shouldBe entries
    nodes.waitForHeightArise()
    sender.getData(newAddress.toAddress.toString) shouldBe entries
  }

  def data(
      entries: List[DataEntry[_]],
      fee: Long = 100000,
      timestamp: Long = System.currentTimeMillis,
      version: TxVersion
  ): DataTransaction =
    DataTransaction.selfSigned(1.toByte, sender.keyPair, entries, fee, timestamp).explicitGet()
}
