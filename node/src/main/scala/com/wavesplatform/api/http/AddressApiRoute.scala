package com.wavesplatform.api.http

import akka.NotUsed
import akka.http.scaladsl.marshalling.{ToResponseMarshallable, ToResponseMarshaller}
import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.server.{Directive0, Route}
import akka.stream.scaladsl.Source
import cats.instances.option.*
import cats.syntax.traverse.*
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonAccountsApi
import com.wavesplatform.api.http.ApiError.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.features.EstimatorProvider.*
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
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import monix.execution.Scheduler
import play.api.libs.json.*

import scala.util.Try

case class AddressApiRoute(
    settings: RestAPISettings,
    wallet: Wallet,
    blockchain: Blockchain,
    transactionPublisher: TransactionPublisher,
    time: Time,
    limitedScheduler: Scheduler,
    routeTimeout: RouteTimeout,
    commonAccountsApi: CommonAccountsApi,
    maxBalanceDepth: Int
) extends ApiRoute
    with AuthRoute
    with TimeLimitedRoute {

  import AddressApiRoute.*

  val MaxAddressesPerRequest = 1000

  override lazy val route: Route =
    pathPrefix("addresses") {
      balanceDetails ~ validate ~ seed ~ balance ~ balances ~ balancesPost ~ balanceWithConfirmations ~ deleteAddress ~
        seq ~ publicKey ~ effectiveBalance ~ effectiveBalanceWithConfirmations ~ getData ~ scriptInfo ~ scriptMeta
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
        "address"              -> address,
        "script"               -> scriptInfoOpt.map(_.script.bytes().base64),
        "scriptText"           -> scriptInfoOpt.map(_.script.expr.toString),
        "version"              -> scriptInfoOpt.map(_.script.stdLibVersion.id),
        "complexity"           -> maxComplexity,
        "verifierComplexity"   -> verifierComplexity,
        "callableComplexities" -> callableComplexities,
        "extraFee"             -> (if (blockchain.hasPaidVerifier(address)) FeeValidation.ScriptExtraFee else 0L),
        "publicKey"            -> scriptInfoOpt.map(_.publicKey.toString)
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

  def balance: Route = (path("balance" / AddrSegment) & get) { address =>
    complete(balanceJson(address))
  }

  def balances: Route = (path("balance") & get & parameters("height".as[Int].?, "address".as[String].*, "asset".?)) {
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
    commonAccountsApi
      .balanceDetails(address)
      .fold(
        e => complete(CustomValidationError(e)),
        { details =>
          import details.*
          complete(
            Json.obj(
              "address"    -> address,
              "regular"    -> regular,
              "generating" -> generating,
              "available"  -> available,
              "effective"  -> effective
            )
          )
        }
      )
  }

  def balanceWithConfirmations: Route = {
    (path("balance" / AddrSegment / IntNumber) & get) { case (address, confirmations) =>
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

  def getData: Route =
    pathPrefix("data" / AddrSegment) { address =>
      implicit val jsonStreamingSupport: ToResponseMarshaller[Source[DataEntry[?], NotUsed]] =
        jacksonStreamMarshaller()(DataEntry.dataEntrySerializer)

      (path(Segment) & get) { key =>
        complete(accountDataEntry(address, key))
      } ~ strictEntity {
        (formField("matches") | parameter("matches")) { matches =>
          Try(matches.r)
            .fold(
              { e =>
                log.trace(s"Error compiling regex $matches: ${e.getMessage}")
                complete(ApiError.fromValidationError(GenericError(s"Cannot compile regex")))
              },
              _ => accountData(address, matches)
            )
        } ~ anyParam("key", limit = settings.dataKeysRequestLimit) { keys =>
          extractMethod.filter(_ != HttpMethods.GET || keys.nonEmpty) { _ =>
            val result = Either
              .cond(keys.nonEmpty, (), DataKeysNotSpecified)
              .map(_ => accountDataList(address, keys.toSeq*))

            complete(result)
          }
        } ~ get {
          accountData(address)
        }
      }
    }

  def root: Route = (path("addresses") & get) {
    complete(wallet.privateKeyAccounts.map(_.toAddress))
  }

  def seq: Route = {
    (path("seq" / IntNumber / IntNumber) & get) { case (start, end) =>
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
    Balance(acc.toString, confirmations, commonAccountsApi.balance(acc, confirmations))
  }

  private def balanceJson(acc: Address) = Balance(acc.toString, 0, commonAccountsApi.balance(acc))

  private def scriptMetaJson(account: Address): Either[ValidationError.ScriptParseError, AccountScriptMeta] = {
    val accountScript = blockchain.accountScript(account)

    accountScript
      .map(_.script)
      .traverse(Global.dAppFuncTypes)
      .map(AccountScriptMeta(account.toString, _))
  }

  private def effectiveBalanceJson(acc: Address, confirmations: Int) = {
    Balance(acc.toString, confirmations, commonAccountsApi.effectiveBalance(acc, confirmations))
  }

  private def validateBalanceDepth(height: Int): Directive0 = {
    if (height < blockchain.height - maxBalanceDepth)
      complete(CustomValidationError(s"Unable to get balance past height ${blockchain.height - maxBalanceDepth}"))
    else
      pass
  }

  private def accountData(address: Address)(implicit m: ToResponseMarshaller[Source[DataEntry[?], NotUsed]]) = {
    routeTimeout.executeFromObservable(
      commonAccountsApi
        .dataStream(address, None)
    )
  }

  private def accountData(addr: Address, regex: String)(implicit m: ToResponseMarshaller[Source[DataEntry[?], NotUsed]]) =
    routeTimeout.executeFromObservable(
      commonAccountsApi
        .dataStream(addr, Some(regex))
    )

  private def accountDataEntry(address: Address, key: String): ToResponseMarshallable =
    commonAccountsApi
      .data(address, key)
      .toRight(DataKeyDoesNotExist)

  private def accountDataList(address: Address, keys: String*) =
    Source.fromIterator(() =>
      keys
        .flatMap(commonAccountsApi.data(address, _))
        .iterator
    )

  def publicKey: Route = (path("publicKey" / PublicKeySegment) & get) { publicKey =>
    complete(Json.obj("address" -> Address.fromPublicKey(publicKey).toString))
  }
}

object AddressApiRoute {
  case class Signed(message: String, publicKey: String, signature: String)

  object Signed {
    import play.api.libs.functional.syntax.*

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

  object AccountScriptMeta extends JsonFormats {
    implicit lazy val accountScriptMetaWrites: Writes[AccountScriptMeta] = Json.writes[AccountScriptMeta]
  }
}
