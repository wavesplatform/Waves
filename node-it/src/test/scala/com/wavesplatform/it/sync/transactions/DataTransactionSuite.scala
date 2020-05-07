package com.wavesplatform.it.sync.transactions

import com.google.common.primitives.Ints
import com.typesafe.config.Config
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.api.http.ApiError.{CustomValidationError, TooBigArrayAllocation}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{TransactionInfo, UnexpectedStatusCodeException}
import com.wavesplatform.it.sync.{calcDataFee, minFee, _}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, DataEntry, EmptyDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.{DataTransaction, TxVersion}
import org.scalatest.{Assertion, Assertions, EitherValues}
import play.api.libs.json._

import scala.concurrent.duration._
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

  val fourthAddress = sender.createAddress()

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    sender.transfer(firstAddress, fourthAddress, 10.waves, minFee, waitForTx = true)
  }

  test("put and remove keys") {
    val address = sender.keyPair.toAddress.stringRepr

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
    val putTxId        = sender.putData(address, putDataEntries, calcDataFee(putDataEntries, TxVersion.V1)).id
    nodes.waitForHeightAriseAndTxPresent(putTxId)

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

    nodes.waitForHeightAriseAndTxPresent(updateAndRemoveTxId)

    sender.getData(address) should contain theSameElementsAs updatedDatEntries ++ putDataEntries.slice(25, 75) ++ newDataEntries

    // can reuse removed keys
    val reusedData = putDataEntries.takeRight(25).map(updateDataEntry)
    val reuseTxId =
      sender.broadcastData(sender.keyPair, reusedData, calcDataFee(reusedData, TxVersion.V1), version = TxVersion.V1).id

    nodes.waitForHeightAriseAndTxPresent(reuseTxId)

    sender.getData(address) should contain theSameElementsAs updatedDatEntries ++ putDataEntries.slice(25, 75) ++ reusedData ++ newDataEntries

    // can't update and remove keys in the same transaction
    val sameKeyEntries = updateAndRemoveDataEntries.tail :+ EmptyDataEntry(updateAndRemoveDataEntries(1).key)
    assertApiError(
      sender.broadcastData(sender.keyPair, sameKeyEntries, calcDataFee(sameKeyEntries, TxVersion.V2), version = TxVersion.V2),
      CustomValidationError("Duplicated keys found")
    )

    //able to "remove" nonexistent key (account state won't be changed, but transaction should be succesfully broadcasted)
    sender.broadcastData(sender.keyPair, List(EmptyDataEntry("nonexistentkey")), calcDataFee(List(EmptyDataEntry("nonexistentkey")), TxVersion.V2), waitForTx = true)
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
    val putDataTxId = sender.putData(address, putDataEntries2, calcDataFee(putDataEntries2, TxVersion.V1) * 10).id
    val removeDataTxId = sender.broadcastData(sender.keyPair, List(EmptyDataEntry("del")), calcDataFee(List(EmptyDataEntry("del")), TxVersion.V2)).id
    nodes.waitForTransaction(putDataTxId)
    nodes.waitForTransaction(removeDataTxId)
    sender.getData(address).filter(_.key == "del") shouldBe List.empty
  }

  test("sender's waves balance is decreased by fee") {
    for (v <- dataTxSupportedVersions) {
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val entry            = IntegerDataEntry("int", 0xcafebabe)
      val data             = List(entry)
      val dataFee          = calcDataFee(data, v)
      val dataTx           = sender.putData(firstAddress, data, version = v, fee = dataFee)
      nodes.waitForHeightAriseAndTxPresent(dataTx.id)
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
      assertBadRequestAndResponse(sender.putData(firstAddress, data, balance1 + 1, version = v), "Accounts balance errors")
      nodes.waitForHeightArise()
      miner.assertBalances(firstAddress, balance1, eff1)

      val leaseAmount = 1.waves
      val leaseId     = sender.lease(firstAddress, secondAddress, leaseAmount, minFee).id
      nodes.waitForHeightAriseAndTxPresent(leaseId)

      assertBadRequestAndResponse(sender.putData(firstAddress, data, balance1 - leaseAmount, version = v), "Accounts balance errors")
      nodes.waitForHeightArise()
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
    //Max size of transaction V1
    val maxKeySizeV1 = 100
    val key          = "\u6fae" * (maxKeySizeV1 - 1)
    val data         = List.tabulate(26)(n => BinaryDataEntry(key + n.toChar, ByteStr(Array.fill(5599)(n.toByte))))
    val fee          = calcDataFee(data, TxVersion.V1)
    val txId         = sender.putData(firstAddress, data, fee, version = TxVersion.V1).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    //Max size of transaction V2
    val maxKeySizeV2 = 400
    val key2         = "u" * (maxKeySizeV2 - 1)
    val data2        = List.tabulate(5)(n => BinaryDataEntry(key2 + n.toChar, ByteStr(Array.fill(Short.MaxValue)(n.toByte))))
    val fee2         = calcDataFee(data2, TxVersion.V2)
    val txId2        = sender.putData(firstAddress, data2, fee2, version = TxVersion.V2).id
    nodes.waitForHeightAriseAndTxPresent(txId2)

  }

  test("data definition and retrieval") {
    for (v <- dataTxSupportedVersions) {
      val txSender = if (v < 2) secondAddress else thirdAddress
      // define first int entry
      val intEntry = IntegerDataEntry("int", 8)
      val intList = List(intEntry)
      val tx1 = sender.putData(txSender, intList, calcDataFee(intList, v), version = v).id
      nodes.waitForHeightAriseAndTxPresent(tx1)

      sender.getDataByKey(txSender, "int") shouldBe intEntry
      sender.getData(txSender) shouldBe intList

      // define boolean entry
      val boolEntry = BooleanDataEntry("bool", true)
      val boolList = List(boolEntry)
      val tx2 = sender.putData(txSender, boolList, calcDataFee(boolList, v), version = v).id
      nodes.waitForHeightAriseAndTxPresent(tx2)

      // define string entry
      val stringEntry = StringDataEntry("str", "AAA")
      val stringList = List(stringEntry)
      val txS = sender.putData(txSender, stringList, calcDataFee(stringList, v), version = v).id
      nodes.waitForHeightAriseAndTxPresent(txS)

      sender.getDataByKey(txSender, "int") shouldBe intEntry
      sender.getDataByKey(txSender, "bool") shouldBe boolEntry
      sender.getDataByKey(txSender, "str") shouldBe stringEntry
      sender.getData(txSender) shouldBe boolList ++ intList ++ stringList

      // redefine int entry
      val reIntEntry = IntegerDataEntry("int", 10)
      val reIntList = List(reIntEntry)
      val tx3 = sender.putData(txSender, reIntList, calcDataFee(reIntList, v), version = v).id
      nodes.waitForHeightAriseAndTxPresent(tx3)

      sender.getDataByKey(txSender, "int") shouldBe reIntEntry
      sender.getDataByKey(txSender, "bool") shouldBe boolEntry
      sender.getData(txSender) shouldBe boolList ++ reIntList ++ stringList

      // define tx with all types
      val (balance2, eff2) = miner.accountBalances(txSender)
      val intEntry2 = IntegerDataEntry("int", -127)
      val boolEntry2 = BooleanDataEntry("bool", false)
      val blobEntry2 = BinaryDataEntry("blob", ByteStr(Array[Byte](127.toByte, 0, 1, 1)))
      val stringEntry2 = StringDataEntry("str", "BBBB")
      val unicodeStringEntry = StringDataEntry("?&$#^123\\/.a:;'\"\r\n\t\u0000|%è&", "specïal")
      val dataAllTypes = List(intEntry2, boolEntry2, blobEntry2, stringEntry2, unicodeStringEntry)
      val fee = calcDataFee(dataAllTypes, v)
      val txId = sender.putData(txSender, dataAllTypes, fee, version = v).id
      nodes.waitForHeightAriseAndTxPresent(txId)

      sender.getDataByKey(txSender, "int") shouldBe intEntry2
      sender.getDataByKey(txSender, "bool") shouldBe boolEntry2
      sender.getDataByKey(txSender, "blob") shouldBe blobEntry2
      sender.getDataByKey(txSender, "str") shouldBe stringEntry2
      sender.getData(txSender) shouldBe dataAllTypes.sortBy(_.key)

      miner.assertBalances(txSender, balance2 - fee, eff2 - fee)

      val json = Json.parse(sender.get(s"/transactions/info/$txId").getResponseBody)
      ((json \ "data") (2) \ "value").as[String].startsWith("base64:") shouldBe true
    }
  }

  test("queries for multiple keys") {
    val tooBigKey = "toobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkey"
    val keys      = Seq("int", "bool", "int", "blob", "?&$#^123\\/.a:;'\"\r\n\t\u0000|%è&", "str", "inexisted_key", tooBigKey)
    val values    = Seq[Any](-127, false, -127, ByteStr(Array[Byte](127.toByte, 0, 1, 1)), "specïal", "BBBB")

    val list     = sender.getDataList(secondAddress, keys: _*).map(_.value)
    val jsonList = sender.getDataListJson(secondAddress, keys: _*).map(_.value)
    val postList = sender.getDataListPost(secondAddress, keys: _*).map(_.value)

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
      val boolData = List(BooleanDataEntry(nonLatinKey, true))
      val boolDataFee = calcDataFee(boolData, v)
      val firstTx = sender.putData(firstAddress, boolData, boolDataFee, version = v).id
      nodes.waitForHeightAriseAndTxPresent(firstTx)
      sender.getDataByKey(firstAddress, nonLatinKey) shouldBe boolData.head

      val longData = List(IntegerDataEntry(nonLatinKey, 100500))
      val longDataFee = calcDataFee(longData, v)
      val secondTx = sender.putData(firstAddress, longData, longDataFee, version = v).id
      nodes.waitForHeightAriseAndTxPresent(secondTx)
      sender.getDataByKey(firstAddress, nonLatinKey) shouldBe longData.head
    }
  }

  test("malformed JSON") {
    for (v <- dataTxSupportedVersions) {
      def request(item: JsObject) = Json.obj("version" -> v, "sender" -> secondAddress, "fee" -> minFee, "data" -> Seq(item))

      val validItem = Json.obj("key" -> "key", "type" -> "integer", "value" -> 8)

      assertBadRequestAndResponse(sender.postJson("/addresses/data", request(validItem - "key")), "key is missing")

      assertBadRequestAndResponse(sender.postJson("/addresses/data", request(validItem - "type")), "type is missing")

      assertBadRequestAndResponse(sender.postJson("/addresses/data", request(validItem + ("type" -> JsString("falafel")))), "unknown type falafel")

      assertBadRequestAndResponse(sender.postJson("/addresses/data", request(validItem - "value")), "value is missing")

      assertBadRequestAndResponse(
        sender.postJson("/addresses/data", request(validItem + ("value" -> JsString("8")))),
        "value is missing or not an integer"
      )

      val notValidIntValue = Json.obj("key" -> "key", "type" -> "integer", "value" -> JsNull)

      assertBadRequestAndResponse(sender.postJson("/addresses/data", request(notValidIntValue)), "value is missing or not an integer")

      val notValidBoolValue = Json.obj("key" -> "bool", "type" -> "boolean", "value" -> JsNull)

      assertBadRequestAndResponse(sender.postJson("/addresses/data", request(notValidBoolValue)), "value is missing or not a boolean")

      assertBadRequestAndResponse(
        sender.postJson("/addresses/data", request(notValidBoolValue + ("value" -> JsString("true")))),
        "value is missing or not a boolean"
      )

      val notValidBlobValue = Json.obj("key" -> "blob", "type" -> "binary", "value" -> JsNull)

      assertBadRequestAndResponse(sender.postJson("/addresses/data", request(notValidBlobValue)), "value is missing or not a string")

      assertBadRequestAndResponse(
        sender.postJson("/addresses/data", request(notValidBlobValue + ("value" -> JsString("base64:not a base64")))),
        "Illegal base64 character"
      )
    }
  }

  test("transaction requires a valid proof") {
    for (v <- dataTxSupportedVersions) {
      def request: JsObject = {
        val rs = sender.postJsonWithApiKey(
          "/transactions/sign",
          Json.obj(
            "version" -> v,
            "type" -> DataTransaction.typeId,
            "sender" -> firstAddress,
            "data" -> List(IntegerDataEntry("int", 333)),
            "fee" -> 100000
          )
        )
        Json.parse(rs.getResponseBody).as[JsObject]
      }

      def id(obj: JsObject) = obj.value("id").as[String]

      val noProof = request - "proofs"
      assertBadRequestAndResponse(sender.postJson("/transactions/broadcast", noProof), "failed to parse json message.*proofs.*missing")
      nodes.foreach(_.ensureTxDoesntExist(id(noProof)))

      val badProof = request ++ Json.obj("proofs" -> Seq(Base58.encode(Array.fill(64)(Random.nextInt.toByte))))
      assertBadRequestAndResponse(sender.postJson("/transactions/broadcast", badProof), "Proof doesn't validate as signature")
      nodes.foreach(_.ensureTxDoesntExist(id(badProof)))

      val withProof = request
      assert((withProof \ "proofs").as[Seq[String]].lengthCompare(1) == 0)
      sender.postJson("/transactions/broadcast", withProof)
      nodes.waitForHeightAriseAndTxPresent(id(withProof))
    }
  }

  test("try to send tx above limits of key, value and entries count") {
    for (v <- dataTxSupportedVersions) {
      val maxKeySize = if (v < 2) 100 else 400
      val maxValueSize = Short.MaxValue
      val maxEntryCount = 100
      val TooBig = "Too big sequences requested"
      val extraKey = "a" * (maxKeySize + 1)
      val data = List(BooleanDataEntry(extraKey, false))

      assertBadRequestAndResponse(sender.putData(firstAddress, data, calcDataFee(data, TxVersion.V1), version = v), TooBig)
      assertBadRequestAndResponse(sender.putData(firstAddress, List(IntegerDataEntry("", 4)), 100000, version = v), "Empty key found")
      assertBadRequestAndResponse(
        sender.putData(firstAddress, List(IntegerDataEntry("abc", 4), IntegerDataEntry("abc", 5)), 100000, version = v),
        "Duplicated keys found"
      )

      val extraValueData = List(BinaryDataEntry("key", ByteStr(Array.fill(maxValueSize + 1)(1.toByte))))
      assertBadRequestAndResponse(sender.putData(firstAddress, extraValueData, 1.waves, version = v), TooBig)
      nodes.waitForHeightArise()

      val largeBinData = List.tabulate(5)(n => BinaryDataEntry(extraKey, ByteStr(Array.fill(maxValueSize)(n.toByte))))
      assertBadRequestAndResponse(sender.putData(firstAddress, largeBinData, 1.waves, version = v), TooBig)
      nodes.waitForHeightArise()

      val largeStrData = List.tabulate(5)(n => StringDataEntry(extraKey, "A" * maxValueSize))
      assertBadRequestAndResponse(sender.putData(firstAddress, largeStrData, 1.waves, version = v), TooBig)
      nodes.waitForHeightArise()

      val tooManyEntriesData = List.tabulate(maxEntryCount + 1)(n => IntegerDataEntry("key", 88))
      assertBadRequestAndResponse(sender.putData(firstAddress, tooManyEntriesData, 1.waves, version = v), TooBig)
      nodes.waitForHeightArise()
    }
  }

  test("try to put empty data") {
    for (v <- dataTxSupportedVersions) {
      val noDataTx = sender.putData(fourthAddress, List.empty, calcDataFee(List.empty, v), version = v).id
      nodes.waitForHeightAriseAndTxPresent(noDataTx)
      sender.getData(fourthAddress) shouldBe List.empty
    }
  }

  test("try to make address with 1000 DataEntries") {
    for (v <- dataTxSupportedVersions) {
      val dataSet = 0 until 200 flatMap (
        i =>
          List(
            IntegerDataEntry(s"int$i", 1000 + i),
            BooleanDataEntry(s"bool$i", false),
            BinaryDataEntry(s"blob$i", ByteStr(Array[Byte](127.toByte, 0, 1, 1))),
            StringDataEntry(s"str$i", s"hi there! + $i"),
            IntegerDataEntry(s"integer$i", 1000 - i)
          )
        )

      val txIds = dataSet.grouped(100).map(_.toList).map(data => sender.putData(fourthAddress, data, calcDataFee(data, v), version = v).id)
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
    val newAddress = sender.createAddress()
    val entries = List(StringDataEntry("test", "test"))
    sender.transfer(firstAddress, newAddress, 2 waves, 1 waves, waitForTx = true)
    sender.broadcastData(pkByAddress(newAddress), entries, 0.1 waves, waitForTx = true)
    sender.getData(newAddress) shouldBe entries
    nodes.waitForHeightArise()
    sender.getData(newAddress) shouldBe entries
  }

  def data(
      entries: List[DataEntry[_]],
      fee: Long = 100000,
      timestamp: Long = System.currentTimeMillis,
      version: TxVersion
  ): DataTransaction =
    DataTransaction.selfSigned(1.toByte, sender.keyPair, entries, fee, timestamp).explicitGet()
}
