package com.wavesplatform.api.http

import java.nio.charset.StandardCharsets
import java.util.regex.Pattern

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.common.CommonAccountApi
import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.crypto
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.lang.contract.meta.Dic
import com.wavesplatform.lang.{Global, ValidationError}
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.protobuf.api
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.{Asset, TransactionFactory}
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import monix.execution.Scheduler
import play.api.libs.json._

import scala.util.{Failure, Success, Try}

case class AddressApiRoute(
    settings: RestAPISettings,
    wallet: Wallet,
    blockchain: Blockchain,
    utxPoolSynchronizer: UtxPoolSynchronizer,
    time: Time,
    limitedScheduler: Scheduler
) extends ApiRoute
    with TimeLimitedRoute
    with BroadcastRoute
    with AuthRoute
    with AutoParamsDirective {

  import AddressApiRoute._

  private[this] val commonAccountApi = new CommonAccountApi(blockchain)
  val MaxAddressesPerRequest         = 1000

  override lazy val route: Route =
    pathPrefix("addresses") {
      validate ~ seed ~ balanceWithConfirmations ~ balanceDetails ~ balance ~ balances ~ balancesPost ~ balanceWithConfirmations ~ verify ~ sign ~ deleteAddress ~ verifyText ~
        signText ~ seq ~ publicKey ~ effectiveBalance ~ effectiveBalanceWithConfirmations ~ getData ~ getDataItem ~ postData ~ scriptInfo ~ scriptMeta
    } ~ root ~ create

  def scriptInfo: Route = (path("scriptInfo" / Segment) & get) { address =>
    completeLimited(
      Address
        .fromString(address)
        .map(addressScriptInfoJson)
    )
  }

  def scriptMeta: Route = (path("scriptInfo" / Segment / "meta") & get) { address =>
    complete(
      Address
        .fromString(address)
        .flatMap(scriptMetaJson)
        .map(ToResponseMarshallable(_))
    )
  }

  def deleteAddress: Route = path(Segment) { address =>
    (delete & withAuth) {
      if (Address.fromString(address).isLeft) {
        complete(InvalidAddress)
      } else {
        val deleted = wallet.findPrivateKey(address).exists(account => wallet.deleteAccount(account))
        complete(Json.obj("deleted" -> deleted))
      }
    }
  }

  def sign: Route = {
    path("sign" / Segment) { address =>
      signPath(address, encode = true)
    }
  }

  def signText: Route = {
    path("signText" / Segment) { address =>
      signPath(address, encode = false)
    }
  }

  def verify: Route = path("verify" / Segment) { address =>
    verifyPath(address, decode = true)
  }

  def verifyText: Route = path("verifyText" / Segment) { address =>
    verifyPath(address, decode = false)
  }

  def balance: Route = (path("balance" / Segment) & get) { address =>
    complete(balanceJson(address))
  }

  def balances: Route = (path("balance") & get & parameters('height.as[Int].?) & parameters('address.*) & parameters('asset.?)) {
    (height, addresses, assetId) =>
      complete(balancesJson(height.getOrElse(blockchain.height), addresses.toSeq, assetId.fold(Waves: Asset)(a => IssuedAsset(Base58.decode(a)))))
  }

  def balancesPost: Route = (path("balance") & (post & entity(as[JsObject]))) { request =>
    val height    = (request \ "height").asOpt[Int]
    val addresses = (request \ "addresses").as[Seq[String]]
    val assetId   = (request \ "asset").asOpt[String]
    complete(balancesJson(height.getOrElse(blockchain.height), addresses, assetId.fold(Waves: Asset)(a => IssuedAsset(Base58.decode(a)))))
  }

  def balanceDetails: Route = (path("balance" / "details" / Segment) & get) { address =>
    complete(
      Address
        .fromString(address)
        .right
        .map(acc => {
          ToResponseMarshallable(balancesDetailsJson(acc))
        })
        .getOrElse(InvalidAddress)
    )
  }

  def balanceWithConfirmations: Route = {
    (path("balance" / Segment / IntNumber) & get) {
      case (address, confirmations) =>
        complete(balanceJson(address, confirmations))
    }
  }

  def effectiveBalance: Route = {
    path("effectiveBalance" / Segment) { address =>
      complete(effectiveBalanceJson(address, 0))
    }
  }

  def effectiveBalanceWithConfirmations: Route = {
    path("effectiveBalance" / Segment / IntNumber) {
      case (address, confirmations) =>
        complete(
          effectiveBalanceJson(address, confirmations)
        )
    }
  }

  def seed: Route = {
    (path("seed" / Segment) & get & withAuth) { address =>
      complete(for {
        pk   <- wallet.findPrivateKey(address)
        seed <- wallet.exportAccountSeed(pk)
      } yield Json.obj("address" -> address, "seed" -> Base58.encode(seed)))
    }
  }

  def validate: Route = (path("validate" / Segment) & get) { address =>
    complete(Validity(address, Address.fromString(address).isRight))
  }

  // TODO: Remove from API
  def postData: Route = (path("data") & withAuth) {
    broadcast[DataRequest](data => TransactionFactory.data(data, wallet, time))
  }

  def getData: Route =
    extractScheduler(
      implicit sc =>
        path("data" / Segment) { address =>
          protobufEntity(api.DataRequest) { request =>
            if (request.matches.nonEmpty)
              complete(
                Try(request.matches.r)
                  .fold(
                    _ => ApiError.fromValidationError(GenericError(s"Cannot compile regex")),
                    r => accountData(address, r.pattern)
                  )
              )
            else complete(accountDataList(address, request.keys: _*))
          } ~ get {
            complete(accountData(address))
          }
        }
    )

  def getDataItem: Route = (path("data" / Segment / Segment) & get) {
    case (address, key) =>
      complete(accountData(address, key))
  }

  def root: Route = (path("addresses") & get) {
    val accounts = wallet.privateKeyAccounts
    val json     = JsArray(accounts.map(a => JsString(a.stringRepr)))
    complete(json)
  }

  def seq: Route = {
    (path("seq" / IntNumber / IntNumber) & get) {
      case (start, end) =>
        if (start >= 0 && end >= 0 && start - end < MaxAddressesPerRequest) {
          val json = JsArray(
            wallet.privateKeyAccounts.map(a => JsString(a.stringRepr)).slice(start, end)
          )

          complete(json)
        } else complete(TooBigArrayAllocation)
    }
  }

  def create: Route = (path("addresses") & post & withAuth) {
    wallet.generateNewAccount() match {
      case Some(pka) => complete(Json.obj("address" -> pka.stringRepr))
      case None      => complete(Unknown)
    }
  }

  private def balancesJson(height: Int, addresses: Seq[String], assetId: Asset): ToResponseMarshallable =
    if (addresses.length > settings.transactionsByAddressLimit) TooBigArrayAllocation
    else if (height < 1 || height > blockchain.height) CustomValidationError(s"Illegal height: $height")
    else {
      implicit val balancesWrites: Writes[(String, Long)] = Writes[(String, Long)] { b =>
        Json.obj("id" -> b._1, "balance" -> b._2)
      }

      val balances = for {
        addressStr <- addresses.toSet[String]
        address    <- Address.fromString(addressStr).toOption
      } yield blockchain.balanceOnlySnapshots(address, height, assetId).map(addressStr -> _._2).getOrElse(addressStr -> 0L)

      ToResponseMarshallable(balances)
    }

  private def balanceJson(address: String, confirmations: Int): ToResponseMarshallable = {
    Address
      .fromString(address)
      .right
      .map(
        acc =>
          ToResponseMarshallable(
            Balance(
              acc.stringRepr,
              confirmations,
              commonAccountApi.balance(acc, confirmations)
            )
          )
      )
      .getOrElse(InvalidAddress)
  }

  private def balanceJson(address: String): ToResponseMarshallable = {
    Address
      .fromString(address)
      .right
      .map(acc => ToResponseMarshallable(Balance(acc.stringRepr, 0, commonAccountApi.balance(acc))))
      .getOrElse(InvalidAddress)
  }

  private def balancesDetailsJson(account: Address): BalanceDetails = {
    val details = commonAccountApi.balanceDetails(account)
    import details._
    BalanceDetails(account.stringRepr, regular, generating, available, effective)
  }

  private def addressScriptInfoJson(account: Address): AddressScriptInfo = {
    val CommonAccountApi.AddressScriptInfo(script, scriptText, complexity, extraFee) = commonAccountApi.script(account)
    AddressScriptInfo(account.stringRepr, script.map(_.base64), scriptText, complexity, extraFee)
  }

  private def scriptMetaJson(account: Address): Either[ValidationError.ScriptParseError, AccountScriptMeta] = {
    import cats.implicits._
    blockchain
      .accountScript(account)
      .traverse(Global.dAppFuncTypes)
      .map(AccountScriptMeta(account.stringRepr, _))
  }

  private def effectiveBalanceJson(address: String, confirmations: Int): ToResponseMarshallable = {
    Address
      .fromString(address)
      .right
      .map(acc => ToResponseMarshallable(Balance(acc.stringRepr, confirmations, commonAccountApi.effectiveBalance(acc, confirmations))))
      .getOrElse(InvalidAddress)
  }

  private def accountData(address: String)(implicit sc: Scheduler): ToResponseMarshallable = {
    Address
      .fromString(address)
      .map { acc =>
        ToResponseMarshallable(commonAccountApi.dataStream(acc).toListL.runAsyncLogErr.map(_.sortBy(_.key)))
      }
      .getOrElse(InvalidAddress)
  }

  private def accountData(address: String, regex: Pattern)(implicit sc: Scheduler): ToResponseMarshallable = {
    Address
      .fromString(address)
      .map { addr =>
        val result: ToResponseMarshallable = commonAccountApi
          .dataStream(addr, k => regex.matcher(k).matches())
          .toListL
          .runAsyncLogErr
          .map(_.sortBy(_.key))

        result
      }
      .getOrElse(InvalidAddress)
  }

  private def accountData(address: String, key: String): ToResponseMarshallable = {
    val result = for {
      addr  <- Address.fromString(address).left.map(_ => InvalidAddress)
      value <- commonAccountApi.data(addr, key).toRight(DataKeyDoesNotExist)
    } yield value
    ToResponseMarshallable(result)
  }

  private def accountDataList(address: String, keys: String*): ToResponseMarshallable = {
    val result = for {
      addr <- Address.fromString(address).left.map(_ => InvalidAddress)
      dataList = keys.flatMap(commonAccountApi.data(addr, _))
    } yield dataList
    ToResponseMarshallable(result)
  }

  private def signPath(address: String, encode: Boolean) = (post & entity(as[String])) { message =>
    withAuth {
      val res = wallet
        .findPrivateKey(address)
        .map(pk => {
          val messageBytes = message.getBytes(StandardCharsets.UTF_8)
          val signature    = crypto.sign(pk, messageBytes)
          val msg          = if (encode) Base58.encode(messageBytes) else message
          Signed(msg, Base58.encode(pk.publicKey), Base58.encode(signature))
        })
      complete(res)
    }
  }

  private def verifyPath(address: String, decode: Boolean): Route = withAuth {
    jsonPost[Signed] { m =>
      if (Address.fromString(address).isLeft) {
        InvalidAddress
      } else {
        //DECODE SIGNATURE
        val msg: Try[Array[Byte]] =
          if (decode) if (m.message.startsWith("base64:")) Base64.tryDecode(m.message) else Base58.tryDecodeWithLimit(m.message, 2048)
          else Success(m.message.getBytes("UTF-8"))
        verifySigned(msg, m.signature, m.publicKey, address)
      }
    }
  }

  private def verifySigned(msg: Try[Array[Byte]], signature: String, publicKey: String, address: String) = {
    (msg, Base58.tryDecodeWithLimit(signature), Base58.tryDecodeWithLimit(publicKey)) match {
      case (Success(msgBytes), Success(signatureBytes), Success(pubKeyBytes)) =>
        val account = PublicKey(pubKeyBytes)
        val isValid = account.stringRepr == address && crypto.verify(signatureBytes, msgBytes, PublicKey(pubKeyBytes))
        Right(Json.obj("valid" -> isValid))
      case _ => Left(InvalidMessage)
    }
  }

  def publicKey: Route = (path("publicKey" / Segment) & get) { publicKey =>
    Base58.tryDecodeWithLimit(publicKey) match {
      case Success(pubKeyBytes) =>
        val account = Address.fromPublicKey(PublicKey(pubKeyBytes))
        complete(Json.obj("address" -> account.stringRepr))
      case Failure(_) => complete(InvalidPublicKey)
    }
  }
}

object AddressApiRoute {
  case class Signed(message: String, publicKey: String, signature: String)

  object Signed {
    import play.api.libs.functional.syntax._

    implicit val signedFormat: Format[Signed] = Format(
      ((JsPath \ "message").read[String] and
        ((JsPath \ "publickey")
          .read[String]
          .orElse((JsPath \ "publicKey").read[String]))
        and (JsPath \ "signature").read[String])(Signed.apply _),
      Json.writes[Signed]
    )
  }

  case class Balance(address: String, confirmations: Int, balance: Long)

  object Balance {
    implicit val balanceFormat: Format[Balance] = Json.format
  }

  case class BalanceDetails(address: String, regular: Long, generating: Long, available: Long, effective: Long)

  object BalanceDetails {
    implicit val balanceDetailsFormat: Format[BalanceDetails] = Json.format
  }

  case class Validity(address: String, valid: Boolean)

  object Validity {
    implicit val validityFormat: Format[Validity] = Json.format
  }

  case class AddressScriptInfo(address: String, script: Option[String], scriptText: Option[String], complexity: Long, extraFee: Long)

  object AddressScriptInfo {
    implicit val accountScriptInfoFormat: Format[AddressScriptInfo] = Json.format
  }

  case class AccountScriptMeta(address: String, meta: Option[Dic])

  object AccountScriptMeta {
    implicit lazy val dicFormat: Writes[Dic]                             = metaConverter.foldRoot
    implicit lazy val accountScriptMetaWrites: Writes[AccountScriptMeta] = Json.writes[AccountScriptMeta]
  }
}
