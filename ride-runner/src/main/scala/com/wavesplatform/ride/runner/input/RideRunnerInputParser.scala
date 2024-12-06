package com.wavesplatform.ride.runner.input

import cats.syntax.either.*
import cats.syntax.option.*
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.*
import com.wavesplatform.account.PublicKeys.EmptyPublicKey
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.lang.script.{Script, ScriptReader}
import com.wavesplatform.ride.ScriptUtil
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.{Asset, TxNonNegativeAmount, TxValidationError}
import pureconfig.*
import play.api.libs.json.*
import com.wavesplatform.ride.runner.input.PureconfigImplicits.*

import java.nio.charset.StandardCharsets
import scala.util.Try

object RideRunnerInputParser {
  val Base58Prefix = "base58:"

  def prepare(config: Config): Config =
    config
      .withFallback(ConfigFactory.parseResources("cli-default-options.conf"))
      .resolve()

  /** Use after "prepare"
    */
  def from(config: Config): RideRunnerInput = {
    val address     = ConfigSource.fromConfig(config).at("address").loadOrThrow[Address]
    val request     = ConfigSource.fromConfig(config).at("request").loadOrThrow[JsObject]
    val chainId     = getChainId(config)
    val intAsString = ConfigSource.fromConfig(config).at("intAsString").load[Boolean].getOrElse(false)
    val trace       = ConfigSource.fromConfig(config).at("trace").load[Boolean].getOrElse(false)
    val evaluateScriptComplexityLimit =
      ConfigSource.fromConfig(config).at("evaluateScriptComplexityLimit").load[Int].getOrElse(Int.MaxValue)
    val maxTxErrorLogSize = ConfigSource.fromConfig(config).at("maxTxErrorLogSize").load[Int].getOrElse(1024)
    val state             = ConfigSource.fromConfig(config).at("state").loadOrThrow[RideRunnerBlockchainState]
    val postProcessing    = ConfigSource.fromConfig(config).at("postProcessing").load[List[RideRunnerPostProcessingMethod]].getOrElse(List.empty)
    val test              = ConfigSource.fromConfig(config).at("test.expected").load[JsValue].map(RideRunnerTest.apply).toOption

    RideRunnerInput(
      address = address,
      request = request,
      chainId = chainId,
      intAsString = intAsString,
      trace = trace,
      evaluateScriptComplexityLimit = evaluateScriptComplexityLimit,
      maxTxErrorLogSize = maxTxErrorLogSize,
      state = state,
      postProcessing = postProcessing,
      test = test
    )
  }

  def getChainId(x: Config): Char = ConfigSource.fromConfig(x).at("chainId").load[Char].getOrElse(fail("chainId is not specified or wrong"))

  implicit val intMapKeyConfigReader: MapKeyConfigReader[Int] = { key =>
    key.toIntOption.getOrElse(fail(s"Expected an integer value between ${Int.MinValue} and ${Int.MaxValue}"))
  }

  implicit val byteStrMapKeyConfigReader: MapKeyConfigReader[ByteStr] = byteStrDefaultBase58FromString(_)

  implicit val addressMapKeyConfigReader: MapKeyConfigReader[Address] = Address.fromString(_).getOrFail

  implicit val issuedAssetMapKeyConfigReader: MapKeyConfigReader[IssuedAsset] = IssuedAsset.fromString(_, identity, fail(_))

  implicit val optBlockIdMapKeyConfigReader: MapKeyConfigReader[Option[BlockId]] = { x =>
    if (x.isEmpty) None else byteStrDefaultBase58FromString(x).some
  }

  implicit val shortMapKeyConfigReader: MapKeyConfigReader[Short] = { key =>
    key.toShortOption.getOrElse(fail(s"Expected an integer value between ${Short.MinValue} and ${Short.MaxValue}"))
  }

  implicit val txNonNegativeAmountConfigReader: ConfigReader[TxNonNegativeAmount] = ConfigReader[Long].map(TxNonNegativeAmount.unsafeFrom)

  implicit val byteStrConfigReader: ConfigReader[ByteStr] = ConfigReader[String].map(byteStrDefaultBase58FromString)

  implicit val stringOrBytesAsByteArrayConfigReader: ConfigReader[StringOrBytesAsByteArray] = ConfigReader[String].map { x =>
    StringOrBytesAsByteArray(byteArrayDefaultUtf8FromString(x))
  }

  implicit val scriptConfigReader: ConfigReader[Script] = ConfigReader[String].map { x =>
    if (x.startsWith(Base64.Prefix)) ScriptReader.fromBytes(Base64.decode(x)).getOrFail
    else ScriptUtil.from(x)
  }

  type SrcOrCompiledScript = Either[String, Script]

  implicit val srcOrCompiledScriptConfigReader: ConfigReader[SrcOrCompiledScript] = ConfigReader[String].map { x =>
    if (x.startsWith(Base64.Prefix)) ScriptReader.fromBytes(Base64.decode(x)).getOrFail.asRight
    else x.asLeft
  }

  implicit val accountConfigReader: ConfigReader[RideRunnerAccount] = ConfigReader.fromCursor { cur =>
    for {
      objCur <- cur.asObjectCursor
      assetBalances <- ConfigReader[Option[Map[IssuedAsset, TxNonNegativeAmount]]]
        .from(objCur.atKeyOrUndefined("assetBalances"))
        .map(_.getOrElse(Map.empty))
      regularBalance    <- ConfigReader[Option[TxNonNegativeAmount]].from(objCur.atKeyOrUndefined("regularBalance"))
      leasing           <- ConfigReader[Option[RideRunnerLeaseBalance]].from(objCur.atKeyOrUndefined("leasing"))
      generatingBalance <- ConfigReader[Option[TxNonNegativeAmount]].from(objCur.atKeyOrUndefined("generatingBalance"))
      data              <- ConfigReader[Option[Map[String, RideRunnerDataEntry]]].from(objCur.atKeyOrUndefined("data"))
      aliases           <- ConfigReader[Option[List[Alias]]].from(objCur.atKeyOrUndefined("aliases")).map(_.getOrElse(Nil))
      scriptInfo        <- ConfigReader[Option[RideRunnerScriptInfo]].from(objCur.atKeyOrUndefined("scriptInfo"))
    } yield RideRunnerAccount(assetBalances, regularBalance, leasing, generatingBalance, data, aliases, scriptInfo)
  }

  implicit val assetConfigReader: ConfigReader[RideRunnerAsset] = ConfigReader.fromCursor { cur =>
    for {
      objCur          <- cur.asObjectCursor
      issuerPublicKey <- ConfigReader[Option[PublicKey]].from(objCur.atKeyOrUndefined("issuerPublicKey")).map(_.getOrElse(EmptyPublicKey))
      name <- ConfigReader[Option[StringOrBytesAsByteArray]].from(objCur.atKeyOrUndefined("name")).map(_.getOrElse(RideRunnerAsset.DefaultName))
      description <- ConfigReader[Option[StringOrBytesAsByteArray]]
        .from(objCur.atKeyOrUndefined("description"))
        .map(_.getOrElse(RideRunnerAsset.DefaultDescription))
      decimals             <- ConfigReader[Option[Int]].from(objCur.atKeyOrUndefined("decimals")).map(_.getOrElse(8))
      reissuable           <- ConfigReader[Option[Boolean]].from(objCur.atKeyOrUndefined("reissuable")).map(_.getOrElse(false))
      quantity             <- ConfigReader[Option[Long]].from(objCur.atKeyOrUndefined("quantity")).map(_.getOrElse(9007199254740991L))
      script               <- ConfigReader[Option[Script]].from(objCur.atKeyOrUndefined("script"))
      minSponsoredAssetFee <- ConfigReader[Option[Long]].from(objCur.atKeyOrUndefined("minSponsoredAssetFee")).map(_.getOrElse(0L))
    } yield RideRunnerAsset(issuerPublicKey, name, description, decimals, reissuable, quantity, script, minSponsoredAssetFee)
  }

  implicit val blockConfigReader: ConfigReader[RideRunnerBlock] = ConfigReader.fromCursor { cur =>
    for {
      objCur <- cur.asObjectCursor
      // Note: `System.currentTimeMillis()` is a side effect, as well as the default value for the `timestamp` field in case class is.
      // It would be a good idea not to use side effects in the default values of case class fields.
      timestamp  <- ConfigReader[Option[Long]].from(objCur.atKeyOrUndefined("timestamp")).map(_.getOrElse(System.currentTimeMillis()))
      baseTarget <- ConfigReader[Option[Long]].from(objCur.atKeyOrUndefined("baseTarget")).map(_.getOrElse(130L))
      generationSignature <- ConfigReader[Option[ByteStr]]
        .from(objCur.atKeyOrUndefined("generationSignature"))
        .map(_.getOrElse(ByteStr(new Array[Byte](64))))
      generatorPublicKey <- ConfigReader[Option[PublicKey]].from(objCur.atKeyOrUndefined("generatorPublicKey")).map(_.getOrElse(EmptyPublicKey))

      vrf         <- ConfigReader[Option[ByteStr]].from(objCur.atKeyOrUndefined("VRF"))
      blockReward <- ConfigReader[Option[Long]].from(objCur.atKeyOrUndefined("blockReward")).map(_.getOrElse(600_000_000L))
    } yield RideRunnerBlock(timestamp, baseTarget, generationSignature, generatorPublicKey, vrf, blockReward)
  }

  implicit val transactionConfigReader: ConfigReader[RideRunnerTransaction] = ConfigReader.fromCursor { cur =>
    for {
      objCur          <- cur.asObjectCursor
      amount          <- ConfigReader[Option[Long]].from(objCur.atKeyOrUndefined("amount")).map(_.getOrElse(1L))
      assetId         <- ConfigReader[Option[Asset]].from(objCur.atKeyOrUndefined("assetId")).map(_.getOrElse(Waves))
      fee             <- ConfigReader[Option[Long]].from(objCur.atKeyOrUndefined("fee")).map(_.getOrElse(100_000L))
      feeAssetId      <- ConfigReader[Option[Asset]].from(objCur.atKeyOrUndefined("feeAssetId")).map(_.getOrElse(Waves))
      recipient       <- objCur.atKey("recipient").flatMap(ConfigReader[AddressOrAlias].from)
      senderPublicKey <- ConfigReader[Option[PublicKey]].from(objCur.atKeyOrUndefined("senderPublicKey")).map(_.getOrElse(EmptyPublicKey))
      height          <- ConfigReader[Option[Int]].from(objCur.atKeyOrUndefined("height"))
      // Note: `System.currentTimeMillis()` is a side effect, as well as the default value for the `timestamp` field in case class is.
      // It would be a good idea not to use side effects in the default values of case class fields.
      timestamp <- ConfigReader[Option[Long]].from(objCur.atKeyOrUndefined("timestamp")).map(_.getOrElse(System.currentTimeMillis()))
      proofs    <- ConfigReader[Option[List[StringOrBytesAsByteArray]]].from(objCur.atKeyOrUndefined("proofs")).map(_.getOrElse(Nil))
      version   <- ConfigReader[Option[Byte]].from(objCur.atKeyOrUndefined("version")).map(_.getOrElse(3: Byte))
      attachment <- ConfigReader[Option[StringOrBytesAsByteArray]]
        .from(objCur.atKeyOrUndefined("attachment"))
        .map(_.getOrElse(StringOrBytesAsByteArray(Array.empty[Byte])))
    } yield RideRunnerTransaction(amount, assetId, fee, feeAssetId, recipient, senderPublicKey, height, timestamp, proofs, version, attachment)
  }

  implicit val aliasConfigReader: ConfigReader[Alias] = ConfigReader[String].map { x =>
    val chainId = AddressScheme.current.chainId

    val separatorNumber = x.count(_ == ':')
    val alias =
      if (separatorNumber == 2) Alias.fromString(x)
      else if (separatorNumber == 1) Alias.createWithChainId(x.substring(x.indexOf(":") + 1), chainId)
      else Alias.createWithChainId(x, chainId)

    alias.flatMap { x => Either.cond(x.chainId == chainId, x, TxValidationError.WrongChain(chainId, x.chainId)) }.getOrFail
  }

  implicit val addressOrAliasConfigReader: ConfigReader[AddressOrAlias] = ConfigReader[String].map { x =>
    val chainId = AddressScheme.current.chainId

    val separatorNumber = x.count(_ == ':')
    val addressOrAlias =
      if (separatorNumber == 2) Alias.fromString(x)
      else if (separatorNumber == 1) Alias.createWithChainId(x.substring(x.indexOf(":") + 1), chainId)
      else Address.fromString(x)

    addressOrAlias.flatMap { x => Either.cond(x.chainId == chainId, x, TxValidationError.WrongChain(chainId, x.chainId)) }.getOrFail
  }

  implicit val publicKeyConfigReader: ConfigReader[PublicKey] = ConfigReader[ByteStr].map(PublicKey(_))

  implicit val rideRunnerDataEntryConfigReader: ConfigReader[RideRunnerDataEntry] = ConfigReader.fromCursor { cur =>
    for {
      objCur   <- cur.asObjectCursor
      dataType <- objCur.atKey("type").flatMap(ConfigReader[String].from)
      data <- dataType match {
        case "integer" => objCur.atKey("value").flatMap(ConfigReader[Long].from).map(IntegerRideRunnerDataEntry.apply)
        case "boolean" => objCur.atKey("value").flatMap(ConfigReader[Boolean].from).map(BooleanRideRunnerDataEntry.apply)
        case "string"  => objCur.atKey("value").flatMap(ConfigReader[String].from).map(StringRideRunnerDataEntry.apply)
        case "binary" =>
          objCur.atKey("value").flatMap(ConfigReader[String].from).map(x => BinaryRideRunnerDataEntry(ByteStr(byteArrayDefaultUtf8FromString(x))))
        case x => fail(s"Expected one of types: integer, boolean, string, binary. Got $x")
      }
    } yield data
  }

  implicit val rideRunnerPostProcessingMethodConfigReader: ConfigReader[RideRunnerPostProcessingMethod] = ConfigReader.fromCursor { cur =>
    for {
      objCur <- cur.asObjectCursor
      method <- objCur.atKey("type").flatMap(ConfigReader[String].from)
      data <- method match {
        case "pick"    => objCur.atKey("path").flatMap(ConfigReader[String].from).map(RideRunnerPostProcessingMethod.Pick.apply)
        case "pickAll" => objCur.atKey("paths").flatMap(ConfigReader[List[String]].from).map(RideRunnerPostProcessingMethod.PickAll.apply)
        case "prune"   => objCur.atKey("paths").flatMap(ConfigReader[List[String]].from).map(RideRunnerPostProcessingMethod.Prune.apply)
        case "regex" =>
          for {
            path    <- objCur.atKey("path").flatMap(ConfigReader[String].from)
            find    <- objCur.atKey("find").flatMap(ConfigReader[String].from)
            replace <- objCur.atKey("replace").flatMap(ConfigReader[String].from)
          } yield RideRunnerPostProcessingMethod.Regex(path, find, replace)
        case x => fail(s"Expected one of types: pick, pickAll, prune. Got $x")
      }
    } yield data
  }

  implicit val rideRunnerScriptInfoConfigReader: ConfigReader[RideRunnerScriptInfo] = ConfigReader.fromCursor { cur =>
    for {
      objCur  <- cur.asObjectCursor
      pk      <- ConfigReader[Option[PublicKey]].from(objCur.atKeyOrUndefined("publicKey")).map(_.getOrElse(EmptyPublicKey))
      imports <- ConfigReader[Option[Map[String, String]]].from(objCur.atKeyOrUndefined("imports")).map(_.getOrElse(Map.empty))
      compiledScript <- ConfigReader[SrcOrCompiledScript].from(objCur.atKeyOrUndefined("script")).map {
        case Right(x)  => x
        case Left(src) => ScriptUtil.from(src, imports)
      }
    } yield RideRunnerScriptInfo(pk, compiledScript)
  }

  private def byteArrayDefaultUtf8FromString(x: String): Array[Byte] = Try {
    if (x.startsWith(Base58Prefix)) Base58.decode(x.substring(7))
    else if (x.startsWith(Base64.Prefix)) Base64.decode(x)
    else x.getBytes(StandardCharsets.UTF_8)
  }.getOrElse(x.getBytes(StandardCharsets.UTF_8))

  private def byteStrDefaultBase58FromString(x: String): ByteStr = ByteStr(byteArrayDefaultBase58FromString(x))
  private def byteArrayDefaultBase58FromString(x: String): Array[Byte] = {
    if (x.startsWith("base64:"))
      Base64.tryDecode(x.substring(7)).fold(e => fail(s"Error parsing base64: ${e.getMessage}", e), identity)
    else if (x.length > Base58.defaultDecodeLimit) fail(s"base58-encoded string length (${x.length}) exceeds maximum length of 192")
    else Base58.tryDecodeWithLimit(x).fold(e => fail(s"Error parsing base58: ${e.getMessage}"), identity)
  }

  private implicit final class ValidationErrorOps[E, T](private val self: Either[E, T]) extends AnyVal {
    def getOrFail: T = self.fold(e => fail(e.toString), identity)
  }

  private def fail(message: String, cause: Throwable = null) = throw new IllegalArgumentException(message, cause)

  implicit def arbitraryKeyMapConfigReader[K, V: ConfigReader](implicit kReader: MapKeyConfigReader[K]): ConfigReader[Map[K, V]] =
    ConfigReader[Map[String, V]].map { xs =>
      xs.map { case (k, v) => kReader.readKey(k) -> v }
    }

  trait MapKeyConfigReader[T] {
    def readKey(key: String): T
  }
}
