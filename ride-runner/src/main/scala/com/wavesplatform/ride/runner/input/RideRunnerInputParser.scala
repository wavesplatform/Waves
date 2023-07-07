package com.wavesplatform.ride.runner.input

import cats.syntax.option.*
import com.google.protobuf.UnsafeByteOperations.unsafeWrap
import com.google.protobuf.{ByteString, UnsafeByteOperations}
import com.wavesplatform.account.{Address, AddressOrAlias, AddressScheme, Alias}
import com.wavesplatform.api.http.utils.UtilsInvocationRequest
import com.wavesplatform.api.http.{DebugApiRoute, requests}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.script.{Script, ScriptReader}
import com.wavesplatform.ride.ScriptUtil
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, AssetScriptInfo, BalanceSnapshot, Height, LeaseBalance, TxMeta}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.transfer.TransferTransactionLike
import com.wavesplatform.transaction.{Asset, Proofs, TransactionFactory, TxPositiveAmount, TxValidationError}
import play.api.libs.json.*
import play.api.libs.json.JsError.toJson

import java.nio.charset.StandardCharsets
import java.util.Locale
import scala.util.Try

object RideRunnerInputParser extends DefaultReads {

  implicit val jsonConfiguration: JsonConfiguration.Aux[Json.WithDefaultValues] = JsonConfiguration[Json.WithDefaultValues](
    discriminator = "type",
    typeNaming = JsonNaming { fullName =>
      fullName.split('.').last.replace("RunnerDataEntry", "").toLowerCase(Locale.US)
    }
  )

  implicit val shortKeyReads: KeyReads[Short] = KeyReads { x =>
    x.toShortOption.fold[JsResult[Short]](mkError("Short"))(JsSuccess(_))
  }

  implicit val intKeyReads: KeyReads[Int] = KeyReads { x =>
    x.toIntOption.fold[JsResult[Int]](mkError("Int"))(JsSuccess(_))
  }

  implicit val byteStrKeyReads: KeyReads[ByteStr] = KeyReads(parseByteStr(_))

  implicit val addressKeyReads: KeyReads[Address] = KeyReads { x =>
    Address.fromString(x).successOrErrorToString("Address")
  }

  implicit val assetKeyReads: KeyReads[Asset] = KeyReads { x =>
    val compatStr = if (x == "WAVES") None else Some(x)
    Try(Asset.fromString(compatStr)).toEither.successOr(e => mkError("Asset", e.getMessage))
  }

  implicit val issuedAssetKeyReads: KeyReads[IssuedAsset] = KeyReads(parseByteStr(_, "IssuedAsset").map(IssuedAsset(_)))

  implicit val optBlockIdKeyReads: KeyReads[Option[BlockId]] = KeyReads { x =>
    if (x.isEmpty) JsSuccess(None) else parseByteStr(x, "Option[BlockId]").map(Some(_))
  }

  implicit def byteArrayReads(hint: String): Reads[Array[Byte]] = StringReads.flatMapResult(parseByteArrayBase58(_, hint))

  implicit val byteStrReads: Reads[ByteStr] = byteArrayReads("ByteStr").map(ByteStr(_))

  implicit val byteStringReads: Reads[ByteString] = byteArrayReads("ByteString").map(unsafeWrap)

  implicit val stringOrBytesAsByteStrReads: Reads[StringOrBytesAsByteStr] = StringReads.flatMapResult { x =>
    JsSuccess(StringOrBytesAsByteStr(ByteStr(decodeBytesFromStrRaw(x))))
  }

  implicit val stringOrBytesAsByteStringReads: Reads[StringOrBytesAsByteString] = StringReads.flatMapResult { x =>
    JsSuccess(StringOrBytesAsByteString(UnsafeByteOperations.unsafeWrap(decodeBytesFromStrRaw(x))))
  }

  implicit val assetReads: Reads[Asset] = StringReads.flatMapResult(assetKeyReads.readKey)

  implicit val paymentReads: Reads[Payment] = Json.reads

  implicit val utilsInvocationRequestReads: Reads[UtilsInvocationRequest] = Json.reads

  implicit val scriptReads: Reads[Script] = StringReads.flatMapResult { x =>
    Try {
      if (x.startsWith(Base64.Prefix)) Base64.decode(x).some
      else none
    }.toEither match {
      case Left(e)            => mkError("Script", e.getMessage)
      case Right(Some(bytes)) => ScriptReader.fromBytes(bytes).successOr(e => mkError("Script", e.m))
      case Right(None)        => Try(ScriptUtil.from(x)).toEither.successOr(e => mkError("Script", e.getMessage))
    }
  }

  implicit val accountScriptInfoReads: Reads[AccountScriptInfo] = Json.reads

  implicit val aliasReads: Reads[Alias] = StringReads.flatMapResult { x =>
    val chainId = AddressScheme.current.chainId

    val separatorNumber = x.count(_ == ':')
    val alias =
      if (separatorNumber == 2) Alias.fromString(x)
      else if (separatorNumber == 1) Alias.createWithChainId(x.substring(x.indexOf(":") + 1), chainId)
      else Alias.createWithChainId(x, chainId)

    alias
      .flatMap { x =>
        Either.cond(x.chainId == chainId, x, TxValidationError.WrongChain(chainId, x.chainId))
      }
      .successOrErrorToString("Alias")
  }

  implicit val addressOrAliasReads: Reads[AddressOrAlias] = StringReads.flatMapResult { x =>
    val chainId = AddressScheme.current.chainId

    val separatorNumber = x.count(_ == ':')
    val addressOrAlias =
      if (separatorNumber == 2) Alias.fromString(x)
      else if (separatorNumber == 1) Alias.createWithChainId(x.substring(x.indexOf(":") + 1), chainId)
      else Address.fromString(x)

    addressOrAlias
      .flatMap { x =>
        Either.cond(x.chainId == chainId, x, TxValidationError.WrongChain(chainId, x.chainId))
      }
      .successOrErrorToString("AddressOrAlias")
  }

  implicit val heightReads: Reads[Height] = Height.lift

  implicit val assetScriptInfoReads: Reads[AssetScriptInfo] = Json.reads

  implicit val assetDescriptionReads: Reads[AssetDescription] = Json.reads

  implicit val blockHeaderReads: Reads[BlockHeader]             = Json.reads
  implicit val signedBlockHeaderReads: Reads[SignedBlockHeader] = Json.reads

  implicit val txMetaReads: Reads[TxMeta] = Json.reads

  implicit val transferTransactionLikeReads: Reads[TransferTransactionLike] = Reads { js =>
    TransactionFactory
      .fromSignedRequest(js)
      .flatMap {
        case tx: TransferTransactionLike => Right(tx)
        case _                           => Left(TxValidationError.UnsupportedTransactionType)
      }
      .successOrErrorToString("TransferTransactionLike")
  }

  implicit val leaseInfoReads: Format[LeaseBalance] = DebugApiRoute.leaseInfoFormat

  implicit val balanceSnapshotReads: Reads[BalanceSnapshot]       = Json.format // format solves "ambiguous" error
  implicit val runnerLeaseBalanceReads: Reads[RunnerLeaseBalance] = Json.reads

  implicit val runnerScriptInfoReads: Reads[RunnerScriptInfo] = Json.reads

  implicit val binaryRunnerDataEntryReads: Reads[BinaryRunnerDataEntry]   = Json.reads
  implicit val booleanRunnerDataEntryReads: Reads[BooleanRunnerDataEntry] = Json.reads
  implicit val integerRunnerDataEntryReads: Reads[IntegerRunnerDataEntry] = Json.reads
  implicit val stringRunnerDataEntryReads: Reads[StringRunnerDataEntry]   = Json.reads

  implicit val runnerDataEntryReads: Reads[RunnerDataEntry] = Json.reads

  implicit val runnerAccountStateReads: Reads[RunnerAccountState] = Json.reads

  implicit val runnerAssetInfoReads: Reads[RunnerAssetInfo] = Json.reads

  implicit val runnerBlockInfoReads: Reads[RunnerBlockInfo] = Json.reads

  implicit val runnerTransactionInfoReads: Reads[RunnerTransactionInfo] = Json.reads

  implicit val stdLibVersionReads: Reads[StdLibVersion] = IntReads.map(StdLibVersion.VersionDic.idMap.apply)

  implicit val txPositiveAmountReads: Reads[TxPositiveAmount] = LongReads.flatMapResult { x =>
    TxPositiveAmount.from(x).successOrErrorToString("TxPositiveAmount")
  }

  implicit val proofsReads: Reads[Proofs] = requests.proofsReads

  implicit val charReads: Reads[Char] = StringReads.flatMapResult { x =>
    if (x.length == 1) JsSuccess(x.head)
    else mkError("Char", s"Expected one char, got: $x")
  }

  implicit val rideRunnerInputReads: Reads[RideRunnerInput] = Json.reads

  def parseByteStr(x: String, hint: String = "ByteStr"): JsResult[ByteStr] = parseByteArrayBase58(x, hint).map(ByteStr(_))

  def parseByteArrayBase58(x: String, hint: String = "Array[Byte]"): JsResult[Array[Byte]] =
    Try {
      if (x.startsWith("base58:")) Base58.decode(x.substring(7))
      else if (x.startsWith(Base64.Prefix)) Base64.decode(x)
      else Base58.decode(x)
    }.toEither.successOr(e => mkError(hint, e.getMessage))

  def decodeBytesFromStrRaw(x: String): Array[Byte] = Try {
    if (x.startsWith("base58:")) Base58.decode(x.substring(7))
    else if (x.startsWith(Base64.Prefix)) Base64.decode(x)
    else x.getBytes(StandardCharsets.UTF_8)
  }.getOrElse(x.getBytes(StandardCharsets.UTF_8))

  def parseJson(x: String): JsValue      = Json.configured.parse(x)
  def parse(x: JsValue): RideRunnerInput = x.as[RideRunnerInput]
  def getChainId(x: JsValue): Char = (x \ "chainId").validate[Char] match {
    case JsSuccess(value, _) => value
    case e: JsError          => throw new IllegalArgumentException(s"Wrong chain id: ${toJson(e)}")
  }

  private implicit final class ValidationErrorOps[E, T](private val self: Either[E, T]) extends AnyVal {
    def successOr(f: E => JsError): JsResult[T]           = self.fold(f, JsSuccess(_))
    def successOrErrorToString(hint: String): JsResult[T] = self.fold(e => mkError(hint, e.toString), JsSuccess(_))
  }

  private def mkError(tpe: String, error: String = "") =
    JsError(Seq(JsPath -> Seq(JsonValidationError(Seq(s"error.expected.$tpe") ++ Seq(error).filterNot(_ == "")))))
}
