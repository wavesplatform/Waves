package com.wavesplatform.it.transactions

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.UnexpectedStatusCodeException
import com.wavesplatform.it.util._
import com.wavesplatform.state2.{BooleanDataEntry, DataEntry, IntegerDataEntry}
import org.scalatest.{Assertion, Assertions}
import play.api.libs.json._
import scorex.api.http.SignedDataRequest
import scorex.crypto.encode.Base58
import scorex.transaction.DataTransaction
import scorex.transaction.TransactionParser.TransactionType

import scala.concurrent.duration._
import scala.util.{Failure, Random, Try}

class DataTransactionSuite extends BaseTransactionSuite {
  private val fee = 100000

  test("sender's waves balance is decreased by fee.") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)

    val data = List(IntegerDataEntry("int", 0xcafebabe))
    val txId = sender.putData(firstAddress, data, fee).id
    nodes.waitForHeightAraiseAndTxPresent(txId)

    notMiner.assertBalances(firstAddress, balance1 - fee, eff1 - fee)
  }

  test("cannot transact without having enough waves") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)

    val data = List(BooleanDataEntry("bool", false))
    assertBadRequestAndResponse(sender.putData(firstAddress, data, balance1 + 1), "negative waves balance")
    nodes.waitForHeightAraise()
    notMiner.assertBalances(firstAddress, balance1, eff1)

    val leaseAmount = 1.waves
    val leaseFee = 100000
    val leaseId = sender.lease(firstAddress, secondAddress, leaseAmount, leaseFee).id
    nodes.waitForHeightAraiseAndTxPresent(leaseId)

    assertBadRequestAndResponse(sender.putData(firstAddress, data, balance1 - leaseAmount), "negative effective balance")
    nodes.waitForHeightAraise()
    notMiner.assertBalances(firstAddress, balance1 - leaseFee, eff1 - leaseAmount - leaseFee)
  }

  test("invalid transaction should not be in UTX or blockchain") {
    def data(entries: List[DataEntry[_]] = List(IntegerDataEntry("int", 177)),
             fee: Long = 100000,
             timestamp: Long = System.currentTimeMillis,
             version: Byte = DataTransaction.Version): DataTransaction =
      DataTransaction.selfSigned(version, sender.privateKey, entries, fee, timestamp).right.get

    def request(tx: DataTransaction): SignedDataRequest =
      SignedDataRequest(DataTransaction.Version, Base58.encode(tx.sender.publicKey), tx.data, tx.fee, tx.timestamp, tx.proofs.base58().toList)

    implicit val w = Json.writes[SignedDataRequest].transform((jsobj: JsObject) =>
      jsobj + ("type" -> JsNumber(TransactionType.DataTransaction.id)))

    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val invalidTxs = Seq(
      (data(timestamp = System.currentTimeMillis + 1.day.toMillis), "Transaction .* is from far future"),
      (data(fee = 99999), "Fee .* does not exceed minimal value"))

    for ((tx, diag) <- invalidTxs) {
      assertBadRequestAndResponse(sender.broadcastRequest(request(tx)), diag)
      nodes.foreach(_.ensureTxDoesntExist(tx.id().base58))
    }

    nodes.waitForHeightAraise()
    notMiner.assertBalances(firstAddress, balance1, eff1)
  }

  test("data definition and retrieval") {
    // define first data item
    val item1 = IntegerDataEntry("int", 8)
    val tx1 = sender.putData(secondAddress, List(item1), fee).id
    nodes.waitForHeightAraiseAndTxPresent(tx1)

    sender.getData(secondAddress, "int") shouldBe item1

    val data1 = sender.getData(secondAddress)
    data1 shouldBe List(item1)

    // define another one
    val item2 = BooleanDataEntry("bool", true)
    val tx2 = sender.putData(secondAddress, List(item2), fee).id
    nodes.waitForHeightAraiseAndTxPresent(tx2)

    sender.getData(secondAddress, "int") shouldBe item1
    sender.getData(secondAddress, "bool") shouldBe item2

    val data2 = sender.getData(secondAddress)
    data2 shouldBe List(item2, item1)

    // redefine item 1
    val item11 = IntegerDataEntry("int", 10)
    val tx3 = sender.putData(secondAddress, List(item11), fee).id
    nodes.waitForHeightAraiseAndTxPresent(tx3)

    sender.getData(secondAddress, "int") shouldBe item11
    sender.getData(secondAddress, "bool") shouldBe item2

    val data3 = sender.getData(secondAddress)
    data3 shouldBe List(item2, item11)
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
    val noDataTx = sender.putData(thirdAddress, List.empty, fee).id
    nodes.waitForHeightAraiseAndTxPresent(noDataTx)
    sender.getData(thirdAddress) shouldBe List.empty

    val key = "intKey"
    val multiKey = List.tabulate(5)(IntegerDataEntry(key, _))
    val twinKeyTx = sender.putData(thirdAddress, multiKey, fee).id
    nodes.waitForHeightAraiseAndTxPresent(twinKeyTx)
    sender.getData(thirdAddress, key) shouldBe IntegerDataEntry(key, multiKey.size - 1)
  }

  test("malformed JSON") {
    def request(item: JsObject) = Json.obj(
      "version" -> 1,
      "sender" -> secondAddress,
      "fee" -> fee,
      "data" -> Seq(item))
    val validItem = Json.obj(
      "key" -> "key",
      "type" -> "integer",
      "value" -> 8)

    assertBadRequestAndResponse(
      sender.postJson("/addresses/data", request(validItem - "key")),
      "key is missing")

    assertBadRequestAndResponse(
      sender.postJson("/addresses/data", request(validItem - "type")),
      "type is missing")

    assertBadRequestAndResponse(
      sender.postJson("/addresses/data", request(validItem + ("type" -> JsString("falafel")))),
      "unknown type falafel")

    assertBadRequestAndResponse(
      sender.postJson("/addresses/data", request(validItem - "value")),
      "value is missing")

    assertBadRequestAndResponse(
      sender.postJson("/addresses/data", request(validItem + ("value" -> JsString("8")))),
      "value is missing or not an integer")
  }

  test("transaction requires a valid proof") {
    def request: JsObject = {
      val rs = sender.postJsonWithApiKey("/transactions/sign", Json.obj(
        "version" -> 1,
        "type" -> TransactionType.DataTransaction.id,
        "sender" -> firstAddress,
        "data" -> List(IntegerDataEntry("int", 333)),
        "fee" -> 100000))
      Json.parse(rs.getResponseBody).as[JsObject]
    }
    def id(obj: JsObject) = obj.value("id").as[String]

    val noProof = request - "proofs"
    assertBadRequestAndMessage(sender.postJson("/transactions/broadcast", noProof), "failed to parse json message")
    nodes.foreach(_.ensureTxDoesntExist(id(noProof)))

    val badProof = request ++ Json.obj("proofs" -> Seq(Base58.encode(Array.fill(64)(Random.nextInt.toByte))))
    assertBadRequestAndResponse(sender.postJson("/transactions/broadcast", badProof), "proof doesn't validate as signature")
    nodes.foreach(_.ensureTxDoesntExist(id(badProof)))

    val withProof = request
    assert((withProof \ "proofs").as[Seq[String]].lengthCompare(1) == 0)
    sender.postJson("/transactions/broadcast", withProof)
    nodes.waitForHeightAraiseAndTxPresent(id(withProof))
  }
}
