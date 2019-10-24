package com.wavesplatform.it.sync.transactions

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.UnexpectedStatusCodeException
import com.wavesplatform.it.sync.{calcDataFee, minFee}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, DataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.DataTransaction
import org.scalatest.{Assertion, Assertions}
import play.api.libs.json._

import scala.concurrent.duration._
import scala.util.{Failure, Random, Try}

//noinspection NameBooleanParameters
class DataTransactionSuite extends BaseTransactionSuite {

  test("sender's waves balance is decreased by fee.") {
    val (balance1, eff1) = miner.accountBalances(firstAddress)
    val entry            = IntegerDataEntry("int", 0xcafebabe)
    val data             = List(entry)
    val transferFee      = calcDataFee(data)
    val txId             = sender.putData(firstAddress, data, transferFee).id
    nodes.waitForHeightAriseAndTxPresent(txId)
    miner.assertBalances(firstAddress, balance1 - transferFee, eff1 - transferFee)
  }

  test("cannot transact without having enough waves") {
    val (balance1, eff1) = miner.accountBalances(firstAddress)

    val data = List(BooleanDataEntry("bool", false))
    assertBadRequestAndResponse(sender.putData(firstAddress, data, balance1 + 1), "negative waves balance")
    nodes.waitForHeightArise()
    miner.assertBalances(firstAddress, balance1, eff1)

    val leaseAmount = 1.waves
    val leaseId     = sender.lease(firstAddress, secondAddress, leaseAmount, minFee).id
    nodes.waitForHeightAriseAndTxPresent(leaseId)

    assertBadRequestAndResponse(sender.putData(firstAddress, data, balance1 - leaseAmount), "negative effective balance")
    nodes.waitForHeightArise()
    miner.assertBalances(firstAddress, balance1 - minFee, eff1 - leaseAmount - minFee)
  }

  test("invalid transaction should not be in UTX or blockchain") {
    def data(
        entries: List[DataEntry[_]] = List(IntegerDataEntry("int", 177)),
        fee: Long = 100000,
        timestamp: Long = System.currentTimeMillis,
        version: Byte = DataTransaction.supportedVersions.head
    ): DataTransaction =
      DataTransaction.selfSigned(sender.privateKey, entries, fee, timestamp).explicitGet()

    val (balance1, eff1) = miner.accountBalances(firstAddress)
    val invalidTxs = Seq(
      (data(timestamp = System.currentTimeMillis + 1.day.toMillis), "Transaction timestamp .* is more than .*ms in the future"),
      (data(fee = 99999), "Fee .* does not exceed minimal value")
    )

    for ((tx, diag) <- invalidTxs) {
      assertBadRequestAndResponse(sender.broadcastRequest(tx.json()), diag)
      nodes.foreach(_.ensureTxDoesntExist(tx.id().base58))
    }

    nodes.waitForHeightArise()
    miner.assertBalances(firstAddress, balance1, eff1)
  }

  test("max transaction size") {
    val key  = "\u6fae" * (DataEntry.MaxKeySize - 1)
    val data = List.tabulate(26)(n => BinaryDataEntry(key + n.toChar, ByteStr(Array.fill(5599)(n.toByte))))
    val fee  = calcDataFee(data)
    val txId = sender.putData(firstAddress, data, fee).id
    nodes.waitForHeightAriseAndTxPresent(txId)
  }

  test("data definition and retrieval") {
    // define first int entry
    val intEntry = IntegerDataEntry("int", 8)
    val intList  = List(intEntry)
    val tx1      = sender.putData(secondAddress, intList, calcDataFee(intList)).id
    nodes.waitForHeightAriseAndTxPresent(tx1)

    sender.getDataByKey(secondAddress, "int") shouldBe intEntry
    sender.getData(secondAddress) shouldBe intList

    // define boolean entry
    val boolEntry = BooleanDataEntry("bool", true)
    val boolList  = List(boolEntry)
    val tx2       = sender.putData(secondAddress, boolList, calcDataFee(boolList)).id
    nodes.waitForHeightAriseAndTxPresent(tx2)

    // define string entry
    val stringEntry = StringDataEntry("str", "AAA")
    val stringList  = List(stringEntry)
    val txS         = sender.putData(secondAddress, stringList, calcDataFee(stringList)).id
    nodes.waitForHeightAriseAndTxPresent(txS)

    sender.getDataByKey(secondAddress, "int") shouldBe intEntry
    sender.getDataByKey(secondAddress, "bool") shouldBe boolEntry
    sender.getDataByKey(secondAddress, "str") shouldBe stringEntry
    sender.getData(secondAddress) shouldBe boolList ++ intList ++ stringList

    // redefine int entry
    val reIntEntry = IntegerDataEntry("int", 10)
    val reIntList  = List(reIntEntry)
    val tx3        = sender.putData(secondAddress, reIntList, calcDataFee(reIntList)).id
    nodes.waitForHeightAriseAndTxPresent(tx3)

    sender.getDataByKey(secondAddress, "int") shouldBe reIntEntry
    sender.getDataByKey(secondAddress, "bool") shouldBe boolEntry
    sender.getData(secondAddress) shouldBe boolList ++ reIntList ++ stringList

    // define tx with all types
    val (balance2, eff2)   = miner.accountBalances(secondAddress)
    val intEntry2          = IntegerDataEntry("int", -127)
    val boolEntry2         = BooleanDataEntry("bool", false)
    val blobEntry2         = BinaryDataEntry("blob", ByteStr(Array[Byte](127.toByte, 0, 1, 1)))
    val stringEntry2       = StringDataEntry("str", "BBBB")
    val unicodeStringEntry = StringDataEntry("?&$#^123\\/.a:;'\"\r\n\t\u0000|%è&", "specïal")
    val dataAllTypes       = List(intEntry2, boolEntry2, blobEntry2, stringEntry2, unicodeStringEntry)
    val fee                = calcDataFee(dataAllTypes)
    val txId               = sender.putData(secondAddress, dataAllTypes, fee).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    sender.getDataByKey(secondAddress, "int") shouldBe intEntry2
    sender.getDataByKey(secondAddress, "bool") shouldBe boolEntry2
    sender.getDataByKey(secondAddress, "blob") shouldBe blobEntry2
    sender.getDataByKey(secondAddress, "str") shouldBe stringEntry2
    sender.getData(secondAddress) shouldBe dataAllTypes.sortBy(_.key)

    miner.assertBalances(secondAddress, balance2 - fee, eff2 - fee)

    val json = Json.parse(sender.get(s"/transactions/info/$txId").getResponseBody)
    ((json \ "data")(2) \ "value").as[String].startsWith("base64:") shouldBe true
  }

  test("queries for multiple keys") {
    val tooBigKey = "toobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkeytoobigkey"
    val keys   = Seq("int", "bool", "int", "blob", "?&$#^123\\/.a:;'\"\r\n\t\u0000|%è&", "str", "inexisted_key", tooBigKey)
    val values = Seq[Any](-127, false, -127, ByteStr(Array[Byte](127.toByte, 0, 1, 1)), "specïal","BBBB")

    val list     = sender.getDataList(secondAddress, keys: _*).map(_.value)
    val jsonList = sender.getDataListJson(secondAddress, keys: _*).map(_.value)
    val postList = sender.getDataListPost(secondAddress, keys: _*).map(_.value)

    list shouldBe values
    jsonList shouldBe list
    postList shouldBe list
  }

  test("queries for nonexistent data") {
    def assertNotFound(url: String): Assertion = Try(sender.get(url)) match {
      case Failure(UnexpectedStatusCodeException(_, _, statusCode, responseBody)) =>
        statusCode shouldBe 404
        responseBody should include("no data for this key")
      case _ => Assertions.fail("Expected 404")
    }

    assertNotFound(s"/addresses/data/$secondAddress/foo")
    assertNotFound(s"/addresses/data/$thirdAddress/foo")
    sender.getData(thirdAddress) shouldBe List.empty
  }

  test("update type for dataEntry") {
    val nonLatinKey = "\u05EA\u05E8\u05D1\u05D5\u05EA, \u05E1\u05E4\u05D5\u05E8\u05D8 \u05D5\u05EA\u05D9\u05D9\u05E8\u05D5\u05EA"
    val boolData    = List(BooleanDataEntry(nonLatinKey, true))
    val boolDataFee = calcDataFee(boolData)
    val firstTx     = sender.putData(firstAddress, boolData, boolDataFee).id
    nodes.waitForHeightAriseAndTxPresent(firstTx)
    sender.getDataByKey(firstAddress, nonLatinKey) shouldBe boolData.head

    val longData    = List(IntegerDataEntry(nonLatinKey, 100500))
    val longDataFee = calcDataFee(longData)
    val secondTx    = sender.putData(firstAddress, longData, longDataFee).id
    nodes.waitForHeightAriseAndTxPresent(secondTx)
    sender.getDataByKey(firstAddress, nonLatinKey) shouldBe longData.head
  }

  test("malformed JSON") {
    def request(item: JsObject) = Json.obj("version" -> 1, "sender"   -> secondAddress, "fee" -> minFee, "data" -> Seq(item))
    val validItem               = Json.obj("key"     -> "key", "type" -> "integer", "value"   -> 8)

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

  test("transaction requires a valid proof") {
    def request: JsObject = {
      val rs = sender.postJsonWithApiKey(
        "/transactions/sign",
        Json.obj(
          "version" -> 1,
          "type"    -> DataTransaction.typeId,
          "sender"  -> firstAddress,
          "data"    -> List(IntegerDataEntry("int", 333)),
          "fee"     -> 100000
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

  test("try to send tx above limits of key, value and size") {
    import DataEntry.{MaxKeySize, MaxValueSize}
    import DataTransaction.MaxEntryCount

    val TooBig   = "Too big sequences requested"
    val extraKey = "a" * (MaxKeySize + 1)
    val data     = List(BooleanDataEntry(extraKey, false))

    assertBadRequestAndResponse(sender.putData(firstAddress, data, calcDataFee(data)), TooBig)
    assertBadRequestAndResponse(sender.putData(firstAddress, List(IntegerDataEntry("", 4)), 100000), "Empty key found")
    assertBadRequestAndResponse(
      sender.putData(firstAddress, List(IntegerDataEntry("abc", 4), IntegerDataEntry("abc", 5)), 100000),
      "Duplicate keys found"
    )

    val extraValueData = List(BinaryDataEntry("key", ByteStr(Array.fill(MaxValueSize + 1)(1.toByte))))
    assertBadRequestAndResponse(sender.putData(firstAddress, extraValueData, 1.waves), TooBig)
    nodes.waitForHeightArise()

    val largeBinData = List.tabulate(5)(n => BinaryDataEntry(extraKey, ByteStr(Array.fill(MaxValueSize)(n.toByte))))
    assertBadRequestAndResponse(sender.putData(firstAddress, largeBinData, 1.waves), TooBig)
    nodes.waitForHeightArise()

    val largeStrData = List.tabulate(5)(n => StringDataEntry(extraKey, "A" * MaxValueSize))
    assertBadRequestAndResponse(sender.putData(firstAddress, largeStrData, 1.waves), TooBig)
    nodes.waitForHeightArise()

    val tooManyEntriesData = List.tabulate(MaxEntryCount + 1)(n => IntegerDataEntry("key", 88))
    assertBadRequestAndResponse(sender.putData(firstAddress, tooManyEntriesData, 1.waves), TooBig)
    nodes.waitForHeightArise()
  }

  test("try to put empty data") {
    val noDataTx = sender.putData(thirdAddress, List.empty, calcDataFee(List.empty)).id
    nodes.waitForHeightAriseAndTxPresent(noDataTx)
    sender.getData(thirdAddress) shouldBe List.empty
  }

  test("try to make address with 1000 DataEntries") {
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

    val txIds = dataSet.grouped(100).map(_.toList).map(data => sender.putData(thirdAddress, data, calcDataFee(data)).id)
    txIds foreach nodes.waitForTransaction

    val r = scala.util.Random.nextInt(199)
    sender.getDataByKey(thirdAddress, s"int$r") shouldBe IntegerDataEntry(s"int$r", 1000 + r)
    sender.getDataByKey(thirdAddress, s"bool$r") shouldBe BooleanDataEntry(s"bool$r", false)
    sender.getDataByKey(thirdAddress, s"blob$r") shouldBe BinaryDataEntry(s"blob$r", ByteStr(Array[Byte](127.toByte, 0, 1, 1)))
    sender.getDataByKey(thirdAddress, s"str$r") shouldBe StringDataEntry(s"str$r", s"hi there! + $r")
    sender.getDataByKey(thirdAddress, s"integer$r") shouldBe IntegerDataEntry(s"integer$r", 1000 - r)

    sender.getData(thirdAddress).size shouldBe 1000
  }
}
