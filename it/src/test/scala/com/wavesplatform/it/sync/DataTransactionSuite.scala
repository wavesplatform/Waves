package com.wavesplatform.it.transactions

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.UnexpectedStatusCodeException
import com.wavesplatform.it.util._
import com.wavesplatform.state2.{BinaryDataEntry, BooleanDataEntry, DataEntry, IntegerDataEntry}
import org.scalatest.{Assertion, Assertions}
import play.api.libs.json._
import scorex.account.AddressOrAlias
import scorex.crypto.encode.Base58
import scorex.transaction.Proofs
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.assets.MassTransferTransaction
import scorex.transaction.assets.MassTransferTransaction.ParsedTransfer
import scorex.transaction.assets.TransferTransaction.MaxAttachmentSize

import scala.concurrent.duration._
import scala.util.{Failure, Try}

class DataTransactionSuite extends BaseTransactionSuite {

  private val assetQuantity = 100.waves
  private val transferAmount = 5.waves
  private val leasingAmount = 5.waves
  private val leasingFee = 0.003.waves
  private val transferFee = notMiner.settings.feesSettings.fees(TransactionType.TransferTransaction.id)(0).fee
  private val massTransferFeePerTransfer = notMiner.settings.feesSettings.fees(TransactionType.MassTransferTransaction.id)(0).fee

  private val fee = 100000


  private def calcFee(data: List[DataEntry[_]]): Long = 100000///

  test("sender's waves balance is decreased by fee.") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)

    val data = List(IntegerDataEntry("int", 0xcafebabe))
    val fee = calcFee(data)
    val txId = sender.putData(firstAddress, data, fee).id
    nodes.waitForHeightAraiseAndTxPresent(txId)

    notMiner.assertBalances(firstAddress, balance1 - fee, eff1 - fee)
  }

  test("cannot transact without having enough waves") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)

    val data = List(BooleanDataEntry("bool", false))
    assertBadRequest2(sender.putData(firstAddress, data, balance1 + 1))
    nodes.waitForHeightAraise()
    notMiner.assertBalances(firstAddress, balance1, eff1)

    val leaseAmount = 1.waves
    val leaseFee = 100000
    val leaseId = sender.lease(firstAddress, secondAddress, leaseAmount, leaseFee).id
    nodes.waitForHeightAraiseAndTxPresent(leaseId)

    assertBadRequest2(sender.putData(firstAddress, data, balance1 - leaseAmount))
    nodes.waitForHeightAraise()
    notMiner.assertBalances(firstAddress, balance1 - leaseFee, eff1 - leaseAmount - leaseFee)
  }

  test("cannot transact with fee less then mininal ") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)

    val data = List(BinaryDataEntry("blob", Base58.decode("mbwana").get))
    assertBadRequest2(sender.putData(firstAddress, data, calcFee(data) / 2))

    nodes.waitForHeightAraise()
    notMiner.assertBalances(firstAddress, balance1, eff1)
  }

//  test("invalid transfer should not be in UTX or blockchain") {
//    import scorex.transaction.assets.MassTransferTransaction.MaxTransferCount
//    val address2 = AddressOrAlias.fromString(secondAddress).right.get
//    val valid = MassTransferTransaction.selfSigned(
//      Proofs.Version, None, sender.privateKey,
//      List(ParsedTransfer(address2, transferAmount)),
//      System.currentTimeMillis,
//      calcFee(1), Array.emptyByteArray).right.get
//    val fromFuture = valid.copy(timestamp = valid.timestamp + 1.day.toMillis)
//    val tooManyTransfers = valid.copy(transfers = List.fill(MaxTransferCount + 1)(ParsedTransfer(address2, 1)), fee = calcFee(MaxTransferCount + 1))
//    val negativeAmountTransfer = valid.copy(transfers = List(ParsedTransfer(address2, -1)))
//    val negativeFee = valid.copy(fee = -1)
//    val longAttachment = valid.copy(attachment = ("ab" * MaxAttachmentSize).getBytes)
//    val invalidTransfers = Seq(fromFuture, tooManyTransfers, negativeAmountTransfer, negativeFee, longAttachment)
//    for (tx <- invalidTransfers) {
//      val id = tx.id()
//      val req = createSignedMassTransferRequest(tx)
//      assertBadRequest2(sender.signedMassTransfer(req))
//      nodes.foreach(_.ensureTxDoesntExist(id.base58))
//    }
//  }

  test("data can be defined and retrieved") {
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

  test("malformed JSON") {
    def request(item: JsObject) = Json.obj(
      "sender" -> secondAddress,
      "fee" -> fee,
      "data" -> Seq(item))
    val validItem = Json.obj(
      "key" -> "key",
      "type" -> "integer",
      "value" -> 8)

    assertBadRequestAndMessageSync(
      sender.postJson("/addresses/data", request(validItem - "key")),
      "key is missing")

    assertBadRequestAndMessageSync(
      sender.postJson("/addresses/data", request(validItem - "type")),
      "type is missing")

    assertBadRequestAndMessageSync(
      sender.postJson("/addresses/data", request(validItem + ("type" -> JsString("falafel")))),
      "unknown type falafel")

    assertBadRequestAndMessageSync(
      sender.postJson("/addresses/data", request(validItem - "value")),
      "value is missing")

    assertBadRequestAndMessageSync(
      sender.postJson("/addresses/data", request(validItem + ("value" -> JsString("8")))),
      "value is missing or not an integer")
  }

//
//  test("huuuge transactions are allowed") {
//    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
//    val fee = calcFee(MaxTransferCount)
//    val amount = (balance1 - fee) / MaxTransferCount
//
//    val transfers = List.fill(MaxTransferCount)(Transfer(firstAddress, amount))
//    val transferId = sender.massTransfer(firstAddress, transfers, fee).id
//
//    nodes.waitForHeightAraiseAndTxPresent(transferId)
//    notMiner.assertBalances(firstAddress, balance1 - fee, eff1 - fee)
//  }
//
//  test("transaction requires either proofs or signature") {
//    val fee = calcFee(2)
//    val transfers = Seq(Transfer(secondAddress, transferAmount), Transfer(thirdAddress, transferAmount))
//    def signedMassTransfer(): JsObject = {
//      val rs = sender.postJsonWithApiKey("/transactions/sign", Json.obj(
//        "type" -> TransactionType.MassTransferTransaction.id,
//        "sender" -> firstAddress,
//        "transfers" -> transfers,
//        "fee" -> fee))
//      Json.parse(rs.getResponseBody).as[JsObject]
//    }
//    def id(obj: JsObject) = obj.value("id").as[String]
//
//    val noProof = signedMassTransfer() - "proofs"
//    assertBadRequest2(sender.postJson("/transactions/broadcast", noProof))
//    nodes.foreach(_.ensureTxDoesntExist(id(noProof)))
//
//    val withProof = signedMassTransfer()
//    assert((withProof \ "proofs").as[Seq[String]].lengthCompare(1) == 0)
//    sender.postJson("/transactions/broadcast", withProof)
//    nodes.waitForHeightAraiseAndTxPresent(id(withProof))
//
//    val signed = signedMassTransfer()
//    val proofs = (signed \ "proofs").as[Seq[String]]
//    assert(proofs.lengthCompare(1) == 0)
//    val withSignature = signed - "proofs" + ("signature" -> JsString(proofs.head))
//    sender.postJson("/transactions/broadcast", withSignature)
//    nodes.waitForHeightAraiseAndTxPresent(id(withSignature))
//  }
//
//  private def createSignedMassTransferRequest(tx: MassTransferTransaction): SignedMassTransferRequest = {
//    import tx._
//    SignedMassTransferRequest(
//      Base58.encode(tx.sender.publicKey),
//      assetId.map(_.base58),
//      transfers.map { case ParsedTransfer(address, amount) => Transfer(address.stringRepr, amount) },
//      fee,
//      timestamp,
//      attachment.headOption.map(_ => Base58.encode(attachment)),
//      proofs.base58().toList
//    )
//  }
//
//  test("try to make mass transfer if use alias for address") {
//
//    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
//    val (balance2, eff2) = notMiner.accountBalances(secondAddress)
//
//    val alias = "masstest_alias"
//
//    val aliasFee = if (!sender.aliasByAddress(secondAddress).exists(_.endsWith(alias))){
//      val aliasId = sender.createAlias(secondAddress, alias, transferFee).id
//      nodes.waitForHeightAraiseAndTxPresent(aliasId)
//      transferFee
//    } else 0
//
//    val aliasFull = sender.aliasByAddress(secondAddress).find(_.endsWith(alias)).get
//
//    val transfers = List(Transfer(firstAddress, 0), Transfer(aliasFull, transferAmount))
//
//    val massTransferTransactionFee = calcFee(transfers.size)
//    val transferId = sender.massTransfer(firstAddress, transfers, massTransferTransactionFee).id
//    nodes.waitForHeightAraiseAndTxPresent(transferId)
//
//    notMiner.assertBalances(firstAddress, balance1 - massTransferTransactionFee - transferAmount, eff1 - massTransferTransactionFee - transferAmount)
//    notMiner.assertBalances(secondAddress, balance2 + transferAmount - aliasFee, eff2 + transferAmount - aliasFee)
//  }
}
