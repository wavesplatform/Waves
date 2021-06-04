package com.wavesplatform.api.http

import akka.NotUsed
import akka.http.scaladsl.marshalling.{ToResponseMarshallable, ToResponseMarshaller}
import akka.http.scaladsl.server.{Directive0, Route}
import akka.stream.scaladsl.Source
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.common.CommonAccountsApi
import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.api.http.requests.DataRequest
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.crypto
import com.wavesplatform.features.EstimatorProvider._
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.meta.FunctionSignatures
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.{Global, ValidationError}
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.state.{Blockchain, DataEntry}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.{Asset, TransactionFactory}
import com.wavesplatform.utils.{Time, _}
import com.wavesplatform.wallet.Wallet
import monix.execution.Scheduler
import play.api.libs.json._

import scala.util.{Success, Try}

case class AddressApiRoute(
    settings: RestAPISettings,
    wallet: Wallet,
    blockchain: Blockchain,
    transactionPublisher: TransactionPublisher,
    time: Time,
    limitedScheduler: Scheduler,
    commonAccountsApi: CommonAccountsApi,
    maxBalanceDepth: Int
) extends ApiRoute
    with BroadcastRoute
    with AuthRoute
    with TimeLimitedRoute {

  import AddressApiRoute._

  val MaxAddressesPerRequest = 1000

  override lazy val route: Route =
    pathPrefix("addresses") {
      balanceDetails ~ validate ~ seed ~ balance ~ balances ~ balancesPost ~ balanceWithConfirmations ~ verify ~ sign ~ deleteAddress ~ verifyText ~
        signText ~ seq ~ publicKey ~ effectiveBalance ~ effectiveBalanceWithConfirmations ~ getData ~ postData ~ scriptInfo ~ scriptMeta
    } ~ root ~ create

  def scriptInfo: Route = (path("scriptInfo" / AddrSegment) & get) { address =>
    completeLimited {
      val scriptInfoOpt = blockchain.accountScript(address)
      val callableComplexitiesOpt =
        for {
          scriptInfo <- scriptInfoOpt
          verifierName = scriptInfo.script match {
            case ContractScriptImpl(_, DApp(_, _, _, Some(vf))) => Some(vf.u.name)
            case _                                              => None
          }
          complexities <- scriptInfo.complexitiesByEstimator.get(blockchain.estimator.version)
        } yield verifierName.fold(complexities)(complexities - _)

      val callableComplexities = callableComplexitiesOpt.getOrElse(Map[String, Long]())
      val verifierComplexity   = scriptInfoOpt.fold(0L)(_.verifierComplexity)
      val maxComplexity        = (callableComplexities.values.toSeq :+ verifierComplexity).max

      Json.obj(
        "address"              -> address.stringRepr,
        "script"               -> scriptInfoOpt.map(_.script.bytes().base64),
        "scriptText"           -> scriptInfoOpt.map(_.script.expr.toString),
        "version"              -> scriptInfoOpt.map(_.script.stdLibVersion.id),
        "complexity"           -> maxComplexity,
        "verifierComplexity"   -> verifierComplexity,
        "callableComplexities" -> callableComplexities,
        "extraFee"             -> (if (blockchain.hasPaidVerifier(address)) FeeValidation.ScriptExtraFee else 0L)
      )
    }
  }

  def scriptMeta: Route = (path("scriptInfo" / AddrSegment / "meta") & get) { address =>
    complete(scriptMetaJson(address))
  }

  def deleteAddress: Route = (delete & withAuth & path(AddrSegment)) { address =>
    val deleted = wallet.privateKeyAccount(address).exists(account => wallet.deleteAccount(account))
    complete(Json.obj("deleted" -> deleted))
  }

  def sign: Route = path("sign" / AddrSegment) { address =>
    signPath(address, encode = true)
  }

  def signText: Route = path("signText" / AddrSegment) { address =>
    signPath(address, encode = false)
  }

  def verify: Route = path("verify" / AddrSegment) { address =>
    verifyPath(address, decode = true)
  }

  def verifyText: Route = path("verifyText" / AddrSegment) { address =>
    verifyPath(address, decode = false)
  }

  def balance: Route = (path("balance" / AddrSegment) & get) { address =>
    complete(balanceJson(address))
  }

  def balances: Route = (path("balance") & get & parameters(("height".as[Int].?, "address".as[String].*, "asset".?))) {
    (maybeHeight, addresses, assetId) =>
      val height = maybeHeight.getOrElse(blockchain.height)
      validateBalanceDepth(height)(
        complete(
          balancesJson(height, addresses.toSeq, assetId.fold(Waves: Asset)(a => IssuedAsset(ByteStr.decodeBase58(a).get)))
        )
      )
  }

  def balancesPost: Route = (path("balance") & (post & entity(as[JsObject]))) { request =>
    val height    = (request \ "height").asOpt[Int].getOrElse(blockchain.height)
    val addresses = (request \ "addresses").as[Seq[String]]
    val assetId   = (request \ "asset").asOpt[String]
    validateBalanceDepth(height)(complete(balancesJson(height, addresses, assetId.fold(Waves: Asset)(a => IssuedAsset(ByteStr.decodeBase58(a).get)))))
  }

  def balanceDetails: Route = (path("balance" / "details" / AddrSegment) & get) { address =>
    val details = commonAccountsApi.balanceDetails(address)
    import details._
    complete(
      Json.obj("address" -> address.stringRepr, "regular" -> regular, "generating" -> generating, "available" -> available, "effective" -> effective)
    )
  }

  def balanceWithConfirmations: Route = {
    (path("balance" / AddrSegment / IntNumber) & get) {
      case (address, confirmations) =>
        validateBalanceDepth(blockchain.height - confirmations)(
          complete(balanceJson(address, confirmations))
        )
    }
  }

  def effectiveBalance: Route = {
    path("effectiveBalance" / AddrSegment) { address =>
      complete(effectiveBalanceJson(address, 0))
    }
  }

  def effectiveBalanceWithConfirmations: Route = {
    path("effectiveBalance" / AddrSegment / IntNumber) { (address, confirmations) =>
      validateBalanceDepth(blockchain.height - confirmations)(
        complete(effectiveBalanceJson(address, confirmations))
      )
    }
  }

  def seed: Route = {
    (path("seed" / Segment) & get & withAuth) { address =>
      complete(for {
        pk   <- wallet.findPrivateKey(address)
        seed <- wallet.exportAccountSeed(pk.toAddress)
      } yield Json.obj("address" -> address, "seed" -> Base58.encode(seed)))
    }
  }

  def validate: Route = (path("validate" / Segment) & get) { addressBytes =>
    complete(Json.obj("address" -> addressBytes, "valid" -> Address.fromString(addressBytes).isRight))
  }

  // TODO: Remove from API
  def postData: Route = (path("data") & withAuth) {
    broadcast[DataRequest](data => TransactionFactory.data(data, wallet, time))
  }

  def getData: Route =
    pathPrefix("data" / AddrSegment) { address =>
      implicit val jsonStreamingSupport: ToResponseMarshaller[Source[JsValue, NotUsed]] = jsonStreamMarshaller()

      (path(Segment) & get) { key =>
        complete(accountDataEntry(address, key))
      } ~ extractScheduler(
        implicit sc =>
          (formField("matches") | parameter("matches")) { matches =>
            Try(matches.r)
              .fold(
                { e =>
                  log.trace(s"Error compiling regex $matches: ${e.getMessage}")
                  complete(ApiError.fromValidationError(GenericError(s"Cannot compile regex")))
                },
                _ => complete(accountData(address, matches))
              )
          } ~ anyParam("key").filter(_.nonEmpty) { keys =>
            complete(accountDataList(address, keys.toSeq: _*))
          } ~ get {
            complete(accountData(address))
          }
      )
    }

  def root: Route = (path("addresses") & get) {
    complete(wallet.privateKeyAccounts.map(_.toAddress))
  }

  def seq: Route = {
    (path("seq" / IntNumber / IntNumber) & get) {
      case (start, end) =>
        if (start < 0 || end < 0 || start > end) complete(GenericError("Invalid sequence"))
        else if (end - start >= MaxAddressesPerRequest) complete(TooBigArrayAllocation(MaxAddressesPerRequest))
        else complete(wallet.privateKeyAccounts.map(_.toAddress).slice(start, end))
    }
  }

  def create: Route = (path("addresses") & post & withAuth) {
    wallet.generateNewAccount() match {
      case Some(pka) => complete(Json.obj("address" -> pka.toAddress))
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
      } yield blockchain.balanceAtHeight(address, height, assetId).fold(addressStr -> 0L)(addressStr -> _._2)

      ToResponseMarshallable(balances)
    }

  private def balanceJson(acc: Address, confirmations: Int) = {
    Balance(acc.stringRepr, confirmations, commonAccountsApi.balance(acc, confirmations))
  }

  private def balanceJson(acc: Address) = Balance(acc.stringRepr, 0, commonAccountsApi.balance(acc))

  private def scriptMetaJson(account: Address): Either[ValidationError.ScriptParseError, AccountScriptMeta] = {
    import cats.implicits._
    val accountScript = blockchain.accountScript(account)

    accountScript
      .map(_.script)
      .traverse(Global.dAppFuncTypes)
      .map(AccountScriptMeta(account.stringRepr, _))
  }

  private def effectiveBalanceJson(acc: Address, confirmations: Int) = {
    Balance(acc.stringRepr, confirmations, commonAccountsApi.effectiveBalance(acc, confirmations))
  }

  private[this] def validateBalanceDepth(height: Int): Directive0 = {
    if (height < blockchain.height - maxBalanceDepth)
      complete(CustomValidationError(s"Unable to get balance past height ${blockchain.height - maxBalanceDepth}"))
    else
      pass
  }

  private def accountData(address: Address)(implicit sc: Scheduler) =
    commonAccountsApi
      .dataStream(address, None)
      .toListL
      .runAsyncLogErr
      .map(data => Source.fromIterator(() => data.sortBy(_.key).iterator.map(Json.toJson[DataEntry[_]])))

  private def accountData(addr: Address, regex: String)(implicit sc: Scheduler) =
    commonAccountsApi
      .dataStream(addr, Some(regex))
      .toListL
      .runAsyncLogErr
      .map(data => Source.fromIterator(() => data.sortBy(_.key).iterator.map(Json.toJson[DataEntry[_]])))

  private def accountDataEntry(address: Address, key: String): ToResponseMarshallable =
    commonAccountsApi.data(address, key).toRight(DataKeyDoesNotExist)

  private def accountDataList(address: Address, keys: String*) =
    Source.fromIterator(() => keys.flatMap(commonAccountsApi.data(address, _)).iterator.map(Json.toJson[DataEntry[_]]))

  private def signPath(address: Address, encode: Boolean): Route = (post & entity(as[String])) { message =>
    withAuth {
      val res = wallet
        .privateKeyAccount(address)
        .map(pk => {
          val messageBytes = message.utf8Bytes
          val signature    = crypto.sign(pk.privateKey, messageBytes)
          val msg          = if (encode) Base58.encode(messageBytes) else message
          Signed(msg, Base58.encode(pk.publicKey.arr), signature.toString)
        })
      complete(res)
    }
  }

  private def verifyPath(address: Address, decode: Boolean): Route = withAuth {
    jsonPost[Signed] { m =>
      val msg: Try[Array[Byte]] =
        if (decode) if (m.message.startsWith("base64:")) Base64.tryDecode(m.message) else Base58.tryDecodeWithLimit(m.message, 2048)
        else Success(m.message.utf8Bytes)
      verifySigned(msg, m.signature, m.publicKey, address)
    }
  }

  private def verifySigned(msg: Try[Array[Byte]], signature: String, publicKey: String, address: Address) = {
    (msg, ByteStr.decodeBase58(signature), Base58.tryDecodeWithLimit(publicKey)) match {
      case (Success(msgBytes), Success(signatureBytes), Success(pubKeyBytes)) =>
        val account = PublicKey(pubKeyBytes)
        val isValid = account.toAddress == address && crypto.verify(signatureBytes, msgBytes, PublicKey(pubKeyBytes))
        Right(Json.obj("valid" -> isValid))
      case _ => Left(InvalidMessage)
    }
  }

  def publicKey: Route = (path("publicKey" / PublicKeySegment) & get) { publicKey =>
    complete(Json.obj("address" -> Address.fromPublicKey(publicKey).stringRepr))
  }
}

object AddressApiRoute {
  case class Signed(message: String, publicKey: String, signature: String)

  object Signed {
    import play.api.libs.functional.syntax._

    implicit val signedFormat: Format[Signed] = Format(
      ((JsPath \ "message").read[String] and
        (JsPath \ "publickey")
          .read[String]
          .orElse((JsPath \ "publicKey").read[String])
        and (JsPath \ "signature").read[String])(Signed.apply _),
      Json.writes[Signed]
    )
  }

  case class Balance(address: String, confirmations: Int, balance: Long)

  object Balance {
    implicit val balanceFormat: Format[Balance] = Json.format
  }

  case class AccountScriptMeta(address: String, meta: Option[FunctionSignatures])

  object AccountScriptMeta {
    implicit lazy val accountScriptMetaWrites: Writes[AccountScriptMeta] = Json.writes[AccountScriptMeta]
  }
}
