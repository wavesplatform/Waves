package com.wavesplatform.ride.runner.input

import cats.syntax.option.*
import com.google.protobuf.{ByteString, UnsafeByteOperations}
import com.typesafe.config.{Config, ConfigRenderOptions}
import com.wavesplatform.account.*
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.json.JsonManipulations
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.script.{Script, ScriptReader}
import com.wavesplatform.ride.ScriptUtil
import com.wavesplatform.state.Height
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.transfer.TransferTransactionLike
import com.wavesplatform.transaction.{TransactionFactory, TxNonNegativeAmount, TxValidationError}
import com.wavesplatform.utils.byteArrayFromString
import net.ceedubs.ficus.Ficus.*
import net.ceedubs.ficus.readers.{ArbitraryTypeReader, ValueReader}
import play.api.libs.json.*

import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.Try

object RideRunnerInputParser extends ArbitraryTypeReader {
  val Base58Prefix = "base58:"

  private def jsValueReader[T: Reads]: ValueReader[T] = { (config: Config, path: String) =>
    // config.getObject(path) doesn't work for primitive values.
    // atPath("x") allows a consistent rendering for all types of content at specified path.
    val fixedPath = if (path == "") "x" else s"x.$path"
    val jsonStr   = config.atPath("x").root().render(ConfigRenderOptions.concise())
    JsonManipulations
      .pick(Json.parse(jsonStr), fixedPath)
      .getOrElse(fail(s"Expected a value at $path"))
      .validate[T] match {
      case JsSuccess(value, _) => value
      case JsError(errors)     => fail(s"Can't parse: ${errors.mkString("\n")}")
    }
  }

  implicit val jsValueValueReader: ValueReader[JsValue]   = jsValueReader
  implicit val jsObjectValueReader: ValueReader[JsObject] = jsValueReader

  implicit val shortMapKeyValueReader: MapKeyValueReader[Short] = { key =>
    key.toShortOption.getOrElse(fail(s"Expected an integer value between ${Short.MinValue} and ${Short.MaxValue}"))
  }

  implicit val intMapKeyValueReader: MapKeyValueReader[Int] = { key =>
    key.toIntOption.getOrElse(fail(s"Expected an integer value between ${Int.MinValue} and ${Int.MaxValue}"))
  }

  implicit val byteStrMapKeyValueReader: MapKeyValueReader[ByteStr] = byteStrDefaultBase58FromString(_)

  implicit val addressMapKeyValueReader: MapKeyValueReader[Address] = Address.fromString(_).getOrFail

  implicit val issuedAssetMapKeyValueReader: MapKeyValueReader[IssuedAsset] = IssuedAsset.fromString(_, identity, fail(_))

  implicit val optBlockIdMapKeyValueReader: MapKeyValueReader[Option[BlockId]] = { x =>
    if (x.isEmpty) None else byteStrDefaultBase58FromString(x).some
  }

  trait MapKeyValueReader[T] {
    def readKey(key: String): T
  }

  implicit def arbitraryKeyMapValueReader[K, V: ValueReader](implicit kReader: MapKeyValueReader[K]): ValueReader[Map[K, V]] =
    ValueReader[Map[String, V]].map { xs =>
      xs.map { case (k, v) => kReader.readKey(k) -> v }
    }

  implicit val byteValueReader: ValueReader[Byte] = ValueReader[Int].map { x =>
    if (x.isValidByte) x.toByte
    else fail(s"Expected an integer value between ${Byte.MinValue} and ${Byte.MaxValue}")
  }

  implicit val txNonNegativeAmountValueReader: ValueReader[TxNonNegativeAmount] = ValueReader[Long].map { x =>
    if (x < 0) fail(s"Expected $x >= 0") else TxNonNegativeAmount.unsafeFrom(x)
  }

  implicit val byteArrayValueReader: ValueReader[Array[Byte]] = ValueReader[String].map(byteArrayFromString(_, identity, fail(_)))

  implicit val byteStringValueReader: ValueReader[ByteString] = byteArrayValueReader.map(UnsafeByteOperations.unsafeWrap)

  implicit val byteStrValueReader: ValueReader[ByteStr] = byteArrayValueReader.map(ByteStr(_))

  implicit val stringOrBytesAsByteArratValueReader: ValueReader[StringOrBytesAsByteArray] = ValueReader[String].map { x =>
    StringOrBytesAsByteArray(byteArrayDefaultUtf8FromString(x))
  }

  implicit val scriptValueReader: ValueReader[Script] = ValueReader[String].map { x =>
    if (x.startsWith(Base64.Prefix)) ScriptReader.fromBytes(Base64.decode(x)).getOrFail
    else ScriptUtil.from(x)
  }

  implicit val aliasValueReader: ValueReader[Alias] = ValueReader[String].map { x =>
    val chainId = AddressScheme.current.chainId

    val separatorNumber = x.count(_ == ':')
    val alias =
      if (separatorNumber == 2) Alias.fromString(x)
      else if (separatorNumber == 1) Alias.createWithChainId(x.substring(x.indexOf(":") + 1), chainId)
      else Alias.createWithChainId(x, chainId)

    alias.flatMap { x => Either.cond(x.chainId == chainId, x, TxValidationError.WrongChain(chainId, x.chainId)) }.getOrFail
  }

  implicit val addressOrAliasValueReader: ValueReader[AddressOrAlias] = ValueReader[String].map { x =>
    val chainId = AddressScheme.current.chainId

    val separatorNumber = x.count(_ == ':')
    val addressOrAlias =
      if (separatorNumber == 2) Alias.fromString(x)
      else if (separatorNumber == 1) Alias.createWithChainId(x.substring(x.indexOf(":") + 1), chainId)
      else Address.fromString(x)

    addressOrAlias.flatMap { x => Either.cond(x.chainId == chainId, x, TxValidationError.WrongChain(chainId, x.chainId)) }.getOrFail
  }

  implicit val addressValueReader: ValueReader[Address] = ValueReader[String].map(Address.fromString(_).getOrFail)

  implicit val publicKeyValueReader: ValueReader[PublicKey] = ValueReader[ByteStr].map(PublicKey(_))

  implicit val heightValueReader: ValueReader[Height] = ValueReader[Int].map(Height(_))

  implicit val transferTransactionLikeValueReader: ValueReader[TransferTransactionLike] = jsObjectValueReader.map { js =>
    TransactionFactory
      .fromSignedRequest(js)
      .flatMap {
        case tx: TransferTransactionLike => Right(tx)
        case _                           => Left(TxValidationError.UnsupportedTransactionType)
      }
      .getOrFail
  }

  implicit val rideRunnerDataEntryValueReader: ValueReader[RideRunnerDataEntry] = ValueReader.relative[RideRunnerDataEntry] { config =>
    config.getString("type") match {
      case "integer" => IntegerRideRunnerDataEntry(config.getLong("value"))
      case "boolean" => BooleanRideRunnerDataEntry(config.getBoolean("value"))
      case "string"  => StringRideRunnerDataEntry(config.getString("value"))
      case "binary"  => BinaryRideRunnerDataEntry(ByteStr(byteArrayDefaultUtf8FromString(config.getString("value"))))
      case x         => fail(s"Expected one of types: integer, boolean, string, binary. Got $x")
    }
  }

  implicit val rideRunnerPostProcessingMethodValueReader: ValueReader[RideRunnerPostProcessingMethod] =
    ValueReader.relative[RideRunnerPostProcessingMethod] { config =>
      config.getString("type") match {
        case "pick"    => PickRideRunnerPostProcessingMethod(config.getString("path"))
        case "pickAll" => PickAllRideRunnerPostProcessingMethod(config.getStringList("paths").asScala.toList)
        case "prune"   => PruneRideRunnerPostProcessingMethod(config.getStringList("paths").asScala.toList)
        case x         => fail(s"Expected one of types: pick, pickAll, prune. Got $x")
      }
    }

  implicit val rideRunnerPostProcessingValueReader: ValueReader[RideRunnerPostProcessing] = arbitraryTypeValueReader[RideRunnerPostProcessing].value

  implicit val rideRunnerAccountValueReader: ValueReader[RideRunnerAccount] = arbitraryTypeValueReader[RideRunnerAccount].value

  implicit val rideRunnerTransactionValueReader: ValueReader[RideRunnerTransaction] = arbitraryTypeValueReader[RideRunnerTransaction].value

  implicit val stdLibVersionValueReader: ValueReader[StdLibVersion] = ValueReader[Int].map(StdLibVersion.VersionDic.idMap.apply)

  implicit val charValueReader: ValueReader[Char] = ValueReader[String].map { x =>
    if (x.length == 1) x.head else fail(s"Expected one char, got: $x")
  }

  implicit val shortValueReader: ValueReader[Short] = ValueReader[Int].map { x =>
    if (x.isValidShort) x.toShort
    else fail(s"Expected a value between ${Short.MinValue} and ${Short.MaxValue}")
  }

  implicit val rideRunnerBlockchainStateValueReader: ValueReader[RideRunnerBlockchainState] =
    arbitraryTypeValueReader[RideRunnerBlockchainState].value

  implicit val rideRunnerTestValueReader: ValueReader[RideRunnerTest] = arbitraryTypeValueReader[RideRunnerTest].value

  implicit val rideRunnerInputValueReader: ValueReader[RideRunnerInput] = arbitraryTypeValueReader[RideRunnerInput].value

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

  def from(config: Config): RideRunnerInput = config.as[RideRunnerInput]

  def getChainId(x: Config): Char = x.getAs[Char]("chainId").getOrElse(fail("chainId is not specified or wrong"))

  private implicit final class ValidationErrorOps[E, T](private val self: Either[E, T]) extends AnyVal {
    def getOrFail: T = self.fold(e => fail(e.toString), identity)
  }

  private def fail(message: String, cause: Throwable = null) = throw new IllegalArgumentException(message, cause)
}
