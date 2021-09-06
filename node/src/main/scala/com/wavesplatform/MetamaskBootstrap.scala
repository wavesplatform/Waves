package com.wavesplatform

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, _}

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpMethods, HttpRequest}
import akka.util.ByteString
import com.google.common.primitives.Longs
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.settings.WalletSettings
import com.wavesplatform.transaction.{ABIConverter, Asset, Transaction}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.utils.EthEncoding
import com.wavesplatform.wallet.Wallet
import play.api.libs.json.{JsObject, Json}

//noinspection ScalaStyle
object MetamaskBootstrap extends App {
  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = 'E'.toByte
  }

  val ethAddressString = args(0)
  val ethAddress       = Address(EthEncoding.toBytes(ethAddressString))
  val wallet           = Wallet(WalletSettings(None, Some("123"), ByteStr.decodeBase58("a3cM").toOption))
  val firstKP          = wallet.generateNewAccount().get

  val transferWavesToEthAddress = TransferTransaction
    .selfSigned(
      2.toByte,
      firstKP,
      ethAddress,
      Asset.Waves,
      50_0000_0000L,
      Asset.Waves,
      10_0000L,
      ByteStr.empty,
      System.currentTimeMillis()
    )
    .explicitGet()

  val massTransfer = MassTransferTransaction
    .selfSigned(
      2.toByte,
      firstKP,
      Asset.Waves,
      Seq(
        MassTransferTransaction.ParsedTransfer(ethAddress, 15_0000_0000L),
        MassTransferTransaction.ParsedTransfer(ethAddress, 16_0000_0000L)
      ),
      20_000L,
      System.currentTimeMillis(),
      ByteStr.empty
    )
    .explicitGet()

  val randomRecipient  = KeyPair(Longs.toByteArray(System.currentTimeMillis()))
  val randomEthAddress = EthEncoding.toHexString(randomRecipient.publicKey.toAddress.publicKeyHash)

  val issue = IssueTransaction
    .selfSigned(2.toByte, firstKP, "ERC20_1", "", 10000_000000L, 6, false, None, 1_00000000, System.currentTimeMillis())
    .explicitGet()

  val assetId = if (args.length > 1) IssuedAsset(ByteStr.decodeBase58(args(1)).get) else issue.asset

  val transferAsset =
    TransferTransaction
      .selfSigned(2.toByte, firstKP, ethAddress, assetId, 50_000000, Waves, 10_000L, ByteStr.empty, System.currentTimeMillis())
      .explicitGet()

  val dAppAccount = wallet.generateNewAccount().get
  val (script, _) = ScriptCompiler
    .compile(
      """
      |{-# STDLIB_VERSION 4 #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
      |{-# CONTENT_TYPE DAPP #-}
      |
      |@Callable (i)
      |func deposit(amount: Int) = {
      |  [
      |    ScriptTransfer(i.caller, amount, unit)
      |  ]
      |}
      |
      |""".stripMargin,
      ScriptEstimatorV3
    )
    .explicitGet()
  val fundDApp = TransferTransaction
    .selfSigned(2.toByte, firstKP, dAppAccount.toAddress, Waves, 1_0000_0000L, Waves, 100000L, ByteStr.empty, System.currentTimeMillis())
    .explicitGet()
  val setDAppScript = SetScriptTransaction.selfSigned(1.toByte, dAppAccount, Some(script), 500000L, System.currentTimeMillis()).explicitGet()

  println(s"""Primary account: $ethAddress / $ethAddressString
       |Asset ID: ${assetId.id} / ${EthEncoding.toHexString(assetId.id.arr.dropRight(12))}
       |DApp: ${dAppAccount.toAddress} / ${EthEncoding.toHexString(dAppAccount.toAddress.publicKeyHash)}""".stripMargin)

  println(Json.prettyPrint(ABIConverter(script).jsonABI))
  println(ABIConverter(script).funcByMethodId)

  implicit val actorSystem = ActorSystem()
  val http                 = Http()

  @tailrec
  def broadcast(tx: Transaction): JsObject = {
    val request = HttpRequest(
      HttpMethods.POST,
      "http://localhost:6869/transactions/broadcast",
      entity = HttpEntity.Strict(ContentTypes.`application/json`, ByteString(tx.json().toString()))
    )
    val result = Await.result(http.singleRequest(request), Duration.Inf)

    val response = Await.result(result.entity.toStrict(10 seconds), Duration.Inf).data.utf8String
    val json     = Json.parse(response).as[JsObject]
    if (result.status.isSuccess() && !json.value.contains("error")) json
    else {
      println(json)
      Thread.sleep(1000)
      broadcast(tx)
    }
  }

  println("Funding primary acc: " + broadcast(transferWavesToEthAddress))
  // if (assetId == issue.asset) println(broadcast(issue))
  // println(broadcast(transferAsset))

  println("Funding DAPP: " + broadcast(fundDApp))
  println("Setting DAPP script: " + broadcast(setDAppScript))
}
