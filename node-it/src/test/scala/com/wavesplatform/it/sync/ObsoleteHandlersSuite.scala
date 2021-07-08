package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.api.http.requests._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.Transaction
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.test._
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import com.wavesplatform.transaction.transfer.TransferTransaction
import org.scalatest.BeforeAndAfterAll
import play.api.libs.json.{Json, Writes}

class ObsoleteHandlersSuite extends BaseTransactionSuite with BeforeAndAfterAll {

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    // explicitly create two more addresses in node's wallet
    sender.postForm("/addresses")
    sender.postForm("/addresses")
  }

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures(BlockchainFeatures.BlockV5.id.toInt -> 0))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

  test("alias create") {
    val json =
      sender.postJsonWithApiKey("/alias/create", CreateAliasRequest("testalias", Some(1.toByte), sender = Some(firstAddress), fee = Some(minFee)))
    val tx = Json.parse(json.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(tx)
  }

  test("assets masstransfer") {
    val fee       = calcMassTransferFee(2)
    val transfers = List(Transfer(secondAddress, 1.waves), Transfer(thirdAddress, 2.waves))
    val json      = sender.postJson("/assets/masstransfer", MassTransferRequest(None, None, firstAddress, transfers, fee))
    val tx        = Json.parse(json.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(tx)
  }

  test("assets transfer") {
    val json = sender.postJson(
      "/assets/transfer",
      TransferRequest(Some(1.toByte), Some(firstAddress), None, secondAddress, None, transferAmount, None, minFee, None, None, None)
    )
    val tx = Json.parse(json.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(tx)
  }

  test("assets issue, burn, reissue, sponsor") {
    val request = Json.obj(
      "sender"      -> firstAddress,
      "name"        -> "testasset",
      "description" -> "testasset",
      "quantity"    -> someAssetAmount,
      "decimals"    -> 2,
      "reissuable"  -> true,
      "fee"         -> issueFee
    )

    val issueJson = sender.postJson("/assets/issue", request)
    val issue     = Json.parse(issueJson.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(issue)

    val burnJson = sender.postJson(
      "/assets/burn",
      Json.obj(
        "sender"  -> firstAddress,
        "assetId" -> issue,
        "amount"  -> someAssetAmount / 2,
        "fee"     -> issueFee
      )
    )
    val burn = Json.parse(burnJson.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(burn)

    val reissueJson = sender.postJson(
      "/assets/reissue",
      Json.obj(
        "sender"     -> firstAddress,
        "assetId"    -> issue,
        "quantity"   -> someAssetAmount,
        "reissuable" -> true,
        "fee"        -> issueFee
      )
    )
    val reissue = Json.parse(reissueJson.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(reissue)

    val sponsorJson = sender.postJson("/assets/sponsor", SponsorFeeRequest(Some(1.toByte), firstAddress, issue, Some(100L), sponsorFee))
    val sponsor     = Json.parse(sponsorJson.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(sponsor)
  }

  test("leasing lease and cancel") {
    val (balance1, eff1) = miner.accountBalances(firstAddress)
    val (balance2, eff2) = miner.accountBalances(secondAddress)

    val leaseJson =
      sender.postJson("/leasing/lease", LeaseRequest(None, Some(firstAddress), None, secondAddress, leasingAmount, minFee, None, None, None))
    val leaseId = Json.parse(leaseJson.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(leaseId)

    miner.assertBalances(firstAddress, balance1 - minFee, eff1 - leasingAmount - minFee)
    miner.assertBalances(secondAddress, balance2, eff2 + leasingAmount)

    val leaseCancelJson = sender.postJson(
      "/leasing/cancel",
      Json.obj(
        "sender"  -> firstAddress,
        "leaseId" -> leaseId,
        "fee"     -> minFee
      )
    )
    val leaseCancel = Json.parse(leaseCancelJson.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(leaseCancel)

    miner.assertBalances(firstAddress, balance1 - 2 * minFee, eff1 - 2 * minFee)
    miner.assertBalances(secondAddress, balance2, eff2)
  }

  test("addresses data") {
    implicit val w: Writes[DataRequest] = Json.writes[DataRequest]
    val data = List(
      IntegerDataEntry("int", 923275292849183L),
      BooleanDataEntry("bool", value = true),
      BinaryDataEntry("blob", ByteStr(Array.tabulate(445)(_.toByte))),
      StringDataEntry("str", "AAA-AAA")
    )
    val fee  = calcDataFee(data, TxVersion.V1)
    val json = sender.postJson("/addresses/data", DataRequest(1.toByte, firstAddress, data, fee))
    val tx   = Json.parse(json.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(tx)
  }

  test("assets broadcast transfer") {
    val json = Json.obj(
      "type"      -> TransferTransaction.typeId,
      "sender"    -> firstAddress,
      "recipient" -> secondAddress,
      "fee"       -> minFee,
      "amount"    -> transferAmount
    )

    val signedRequestResponse = sender.postJsonWithApiKey(s"/transactions/sign/$firstAddress", json)
    val transfer              = Json.parse(signedRequestResponse.getResponseBody).as[TransferRequest]

    val transferIdJson = sender.postJson("/assets/broadcast/transfer", transfer)
    val transferId     = Json.parse(transferIdJson.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(transferId)
  }

  test("leasing broadcast lease and cancel") {
    val jsonL = Json.obj(
      "type"      -> LeaseTransaction.typeId,
      "sender"    -> firstAddress,
      "recipient" -> secondAddress,
      "fee"       -> minFee,
      "amount"    -> transferAmount
    )

    val r1    = sender.postJsonWithApiKey(s"/transactions/sign/$firstAddress", jsonL)
    val lease = Json.parse(r1.getResponseBody).as[LeaseRequest]

    val leaseIdJson = sender.postJson("/leasing/broadcast/lease", lease)
    val leaseId     = Json.parse(leaseIdJson.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(leaseId)

    val jsonLC = Json.obj(
      "type"    -> LeaseCancelTransaction.typeId,
      "sender"  -> firstAddress,
      "fee"     -> minFee,
      "leaseId" -> leaseId
    )

    val r2     = sender.postJsonWithApiKey(s"/transactions/sign/$firstAddress", jsonLC)
    val leaseC = Json.parse(r2.getResponseBody).as[LeaseCancelRequest]

    val leaseCIdJson = sender.postJson("/leasing/broadcast/cancel", leaseC)
    val leaseCId     = Json.parse(leaseCIdJson.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(leaseCId)
  }

}
