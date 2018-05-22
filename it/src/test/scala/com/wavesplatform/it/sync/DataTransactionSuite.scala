package com.wavesplatform.it.sync

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.UnexpectedStatusCodeException
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, ByteStr, DataEntry, LongDataEntry, StringDataEntry}
import org.scalatest.{Assertion, Assertions}
import play.api.libs.json._
import scorex.api.http.SignedDataRequest
import com.wavesplatform.utils.Base58
import scorex.transaction.DataTransaction

import scala.concurrent.duration._
import scala.util.{Failure, Random, Try}

class DataTransactionSuite extends BaseTransactionSuite {
  private val fee = 100000

  test("sender's waves balance is decreased by fee.") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val entry            = LongDataEntry("int", 0xcafebabe)
    val data             = List(entry)
    val transferFee      = calcDataFee(data)
    val txId             = sender.putData(firstAddress, data, transferFee).id
    nodes.waitForHeightAriseAndTxPresent(txId)
    notMiner.assertBalances(firstAddress, balance1 - transferFee, eff1 - transferFee)
  }

  test("cannot transact without having enough waves") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)

    val data = List(BooleanDataEntry("bool", false))
    assertBadRequestAndResponse(sender.putData(firstAddress, data, balance1 + 1), "negative waves balance")
    nodes.waitForHeightArise()
    notMiner.assertBalances(firstAddress, balance1, eff1)

    val leaseAmount = 1.waves
    val leaseFee    = 100000
    val leaseId     = sender.lease(firstAddress, secondAddress, leaseAmount, leaseFee).id
    nodes.waitForHeightAriseAndTxPresent(leaseId)

    assertBadRequestAndResponse(sender.putData(firstAddress, data, balance1 - leaseAmount), "negative effective balance")
    nodes.waitForHeightArise()
    notMiner.assertBalances(firstAddress, balance1 - leaseFee, eff1 - leaseAmount - leaseFee)
  }

  test("invalid transaction should not be in UTX or blockchain") {
    def data(entries: List[DataEntry[_]] = List(LongDataEntry("int", 177)),
             fee: Long = 100000,
             timestamp: Long = System.currentTimeMillis,
             version: Byte = DataTransaction.supportedVersions.head): DataTransaction =
      DataTransaction.selfSigned(version, sender.privateKey, entries, fee, timestamp).right.get

    def request(tx: DataTransaction): SignedDataRequest =
      SignedDataRequest(DataTransaction.supportedVersions.head,
                        Base58.encode(tx.sender.publicKey),
                        tx.data,
                        tx.fee,
                        tx.timestamp,
                        tx.proofs.base58().toList)

    implicit val w = Json.writes[SignedDataRequest].transform((jsobj: JsObject) => jsobj + ("type" -> JsNumber(DataTransaction.typeId)))

    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val invalidTxs = Seq(
      (data(timestamp = System.currentTimeMillis + 1.day.toMillis), "Transaction .* is from far future"),
      (data(fee = 99999), "Fee .* does not exceed minimal value")
    )

    for ((tx, diag) <- invalidTxs) {
      assertBadRequestAndResponse(sender.broadcastRequest(request(tx)), diag)
      nodes.foreach(_.ensureTxDoesntExist(tx.id().base58))
    }

    nodes.waitForHeightArise()
    notMiner.assertBalances(firstAddress, balance1, eff1)
  }

  test("max transaction size") {
    import DataEntry.MaxKeySize

    val maxKey = "\u6fae" * MaxKeySize
    val data   = List.tabulate(26)(n => BinaryDataEntry("\u6fae" * MaxKeySize, ByteStr(Array.fill(5598)(n.toByte))))
    val fee    = calcDataFee(data)
    val txId   = sender.putData(firstAddress, data, fee).id

    nodes.waitForHeightAriseAndTxPresent(txId)
    sender.getData(firstAddress, maxKey) shouldBe data.last
  }

  test("data definition and retrieval") {
    // define first int entry
    val intEntry = LongDataEntry("int", 8)
    val intList  = List(intEntry)
    val tx1      = sender.putData(secondAddress, intList, calcDataFee(intList)).id
    nodes.waitForHeightAriseAndTxPresent(tx1)

    sender.getData(secondAddress, "int") shouldBe intEntry
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

    sender.getData(secondAddress, "int") shouldBe intEntry
    sender.getData(secondAddress, "bool") shouldBe boolEntry
    sender.getData(secondAddress, "str") shouldBe stringEntry
    sender.getData(secondAddress) shouldBe boolList ++ intList ++ stringList

    // redefine int entry
    val reIntEntry = LongDataEntry("int", 10)
    val reIntList  = List(reIntEntry)
    val tx3        = sender.putData(secondAddress, reIntList, calcDataFee(reIntList)).id
    nodes.waitForHeightAriseAndTxPresent(tx3)

    sender.getData(secondAddress, "int") shouldBe reIntEntry
    sender.getData(secondAddress, "bool") shouldBe boolEntry
    sender.getData(secondAddress) shouldBe boolList ++ reIntList ++ stringList

    // define tx with all types
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)
    val intEntry2        = LongDataEntry("int", -127)
    val boolEntry2       = BooleanDataEntry("bool", false)
    val blobEntry2       = BinaryDataEntry("blob", ByteStr(Array[Byte](127.toByte, 0, 1, 1)))
    val stringEntry2     = StringDataEntry("str", "BBBB")
    val dataAllTypes     = List(intEntry2, boolEntry2, blobEntry2, stringEntry2)
    val fee              = calcDataFee(dataAllTypes)
    val txId             = sender.putData(secondAddress, dataAllTypes, fee).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    sender.getData(secondAddress, "int") shouldBe intEntry2
    sender.getData(secondAddress, "bool") shouldBe boolEntry2
    sender.getData(secondAddress, "blob").equals(blobEntry2)
    sender.getData(secondAddress, "str").equals(stringEntry2)
    sender.getData(secondAddress).equals(dataAllTypes)

    notMiner.assertBalances(secondAddress, balance2 - fee, eff2 - fee)
  }

  test("queries for nonexistent data") {
    def assertNotFound(url: String): Assertion = Try(sender.get(url)) match {
      case Failure(UnexpectedStatusCodeException(_, statusCode, responseBody)) =>
        statusCode shouldBe 404
        responseBody should include("no data for this key")
      case _ => Assertions.fail("Expected 404")
    }

    assertNotFound(s"/addresses/data/$secondAddress/foo")
    assertNotFound(s"/addresses/data/$thirdAddress/foo")
    sender.getData(thirdAddress) shouldBe List.empty
  }

  test("data definition corner cases") {
    val noDataTx = sender.putData(thirdAddress, List.empty, calcDataFee(List.empty)).id
    nodes.waitForHeightAriseAndTxPresent(noDataTx)
    sender.getData(thirdAddress) shouldBe List.empty

    val emptyKey   = List(LongDataEntry("", 7))
    val emptyKeyTx = sender.putData(thirdAddress, emptyKey, fee).id
    nodes.waitForHeightAriseAndTxPresent(emptyKeyTx)
    sender.getData(thirdAddress, "") shouldBe emptyKey.head

    val latinKey    = "C,u!"
    val multiKey    = List.tabulate(5)(LongDataEntry(latinKey, _))
    val multiKeyFee = calcDataFee(multiKey)
    val multiKeyTx  = sender.putData(thirdAddress, multiKey, multiKeyFee).id
    nodes.waitForHeightAriseAndTxPresent(multiKeyTx)
    sender.getData(thirdAddress, latinKey) shouldBe multiKey.last

    val nonLatinKey   = "\u05EA\u05E8\u05D1\u05D5\u05EA, \u05E1\u05E4\u05D5\u05E8\u05D8 \u05D5\u05EA\u05D9\u05D9\u05E8\u05D5\u05EA"
    val typeChange    = List(BooleanDataEntry(nonLatinKey, true))
    val typeChangeFee = calcDataFee(typeChange)
    val typeChangeTx  = sender.putData(thirdAddress, typeChange, typeChangeFee).id
    nodes.waitForHeightAriseAndTxPresent(typeChangeTx)
    sender.getData(thirdAddress, nonLatinKey) shouldBe typeChange.head
  }

  test("malformed JSON") {
    def request(item: JsObject) = Json.obj("version" -> 1, "sender"   -> secondAddress, "fee" -> fee, "data" -> Seq(item))
    val validItem               = Json.obj("key"     -> "key", "type" -> "integer", "value"   -> 8)

    assertBadRequestAndResponse(sender.postJson("/addresses/data", request(validItem - "key")), "key is missing")

    assertBadRequestAndResponse(sender.postJson("/addresses/data", request(validItem - "type")), "type is missing")

    assertBadRequestAndResponse(sender.postJson("/addresses/data", request(validItem + ("type" -> JsString("falafel")))), "unknown type falafel")

    assertBadRequestAndResponse(sender.postJson("/addresses/data", request(validItem - "value")), "value is missing")

    assertBadRequestAndResponse(sender.postJson("/addresses/data", request(validItem + ("value" -> JsString("8")))),
                                "value is missing or not an integer")

    val notValidIntValue = Json.obj("key" -> "key", "type" -> "integer", "value" -> JsNull)

    assertBadRequestAndResponse(sender.postJson("/addresses/data", request(notValidIntValue)), "value is missing or not an integer")

    val notValidBoolValue = Json.obj("key" -> "bool", "type" -> "boolean", "value" -> JsNull)

    assertBadRequestAndResponse(sender.postJson("/addresses/data", request(notValidBoolValue)), "value is missing or not a boolean")

    assertBadRequestAndResponse(sender.postJson("/addresses/data", request(notValidBoolValue + ("value" -> JsString("true")))),
                                "value is missing or not a boolean")

    val notValidBlobValue = Json.obj("key" -> "blob", "type" -> "binary", "value" -> JsNull)

    assertBadRequestAndResponse(sender.postJson("/addresses/data", request(notValidBlobValue)), "value is missing or not a string")

    assertBadRequestAndResponse(sender.postJson("/addresses/data", request(notValidBlobValue + ("value" -> JsString("base64:not a base64")))),
                                "Illegal base64 character")

    assertBadRequestAndResponse(sender.postJson("/addresses/data", request(notValidBlobValue + ("value" -> JsString("yomp")))),
                                "Base64 encoding expected")
  }

  test("transaction requires a valid proof") {
    def request: JsObject = {
      val rs = sender.postJsonWithApiKey(
        "/transactions/sign",
        Json.obj("version" -> 1,
                 "type"    -> DataTransaction.typeId,
                 "sender"  -> firstAddress,
                 "data"    -> List(LongDataEntry("int", 333)),
                 "fee"     -> 100000)
      )
      Json.parse(rs.getResponseBody).as[JsObject]
    }
    def id(obj: JsObject) = obj.value("id").as[String]

    val noProof = request - "proofs"
    assertBadRequestAndResponse(sender.postJson("/transactions/broadcast", noProof), "failed to parse json message.*proofs.*missing")
    nodes.foreach(_.ensureTxDoesntExist(id(noProof)))

    val badProof = request ++ Json.obj("proofs" -> Seq(Base58.encode(Array.fill(64)(Random.nextInt.toByte))))
    assertBadRequestAndResponse(sender.postJson("/transactions/broadcast", badProof), "proof doesn't validate as signature")
    nodes.foreach(_.ensureTxDoesntExist(id(badProof)))

    val withProof = request
    assert((withProof \ "proofs").as[Seq[String]].lengthCompare(1) == 0)
    sender.postJson("/transactions/broadcast", withProof)
    nodes.waitForHeightAriseAndTxPresent(id(withProof))
  }

  test("try to send tx above limits of key, value and size") {
    import DataEntry.{MaxKeySize, MaxValueSize}
    import DataTransaction.MaxEntryCount

    val message  = "Too big sequences requested"
    val extraKey = "a" * (MaxKeySize + 1)
    val data     = List(BooleanDataEntry(extraKey, false))

    assertBadRequestAndResponse(sender.putData(firstAddress, data, calcDataFee(data)), message)
    nodes.waitForHeightArise()

    val extraValueData = List(BinaryDataEntry("key", ByteStr(Array.fill(MaxValueSize + 1)(1.toByte))))
    assertBadRequestAndResponse(sender.putData(firstAddress, extraValueData, calcDataFee(extraValueData)), message)
    nodes.waitForHeightArise()

    val largeBinData = List.tabulate(5)(n => BinaryDataEntry(extraKey, ByteStr(Array.fill(MaxValueSize)(n.toByte))))
    assertBadRequestAndResponse(sender.putData(firstAddress, largeBinData, calcDataFee(largeBinData)), message)
    nodes.waitForHeightArise()

    val largeStrData = List.tabulate(5)(n => StringDataEntry(extraKey, "A" * MaxValueSize))
    assertBadRequestAndResponse(sender.putData(firstAddress, largeStrData, calcDataFee(largeStrData)), message)
    nodes.waitForHeightArise()

    val tooManyEntriesData = List.tabulate(MaxEntryCount + 1)(n => LongDataEntry("key", 88))
    assertBadRequestAndResponse(sender.putData(firstAddress, tooManyEntriesData, calcDataFee(tooManyEntriesData)), message)
    nodes.waitForHeightArise()
  }
}
