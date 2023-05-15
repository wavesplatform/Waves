package com.wavesplatform.ride.runner.input

import com.google.protobuf.ByteString
import com.google.protobuf.UnsafeByteOperations.unsafeWrap
import com.wavesplatform.account.{Address, AddressOrAlias, AddressScheme, Alias}
import com.wavesplatform.api.http.{DebugApiRoute, requests}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.script.{Script, ScriptReader}
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, AssetScriptInfo, BalanceSnapshot, Height, TxMeta}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.transfer.TransferTransactionLike
import com.wavesplatform.transaction.{Asset, Proofs, TransactionFactory, TxPositiveAmount, TxValidationError}
import play.api.libs.json.*

import java.util.Locale
import scala.util.Try

object RideRunnerJson extends DefaultReads {

  implicit val jsonConfiguration = JsonConfiguration[Json.WithDefaultValues](
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

  implicit def byteArrayReads(hint: String) = StringReads.flatMapResult(parseByteArray(_, hint))

  implicit val byteStrReads = byteArrayReads("ByteStr").map(ByteStr(_))

  implicit val byteStringReads: Reads[ByteString] = byteArrayReads("ByteString").map(unsafeWrap)

  implicit val scriptReads: Reads[Script] = StringReads.flatMapResult { x =>
    parseByteArray(x, "Script").flatMap { bytes =>
      ScriptReader.fromBytes(bytes).successOr(e => mkError("Script", e.m))
    }
  }

  implicit val accountScriptInfoReads: Reads[AccountScriptInfo] = Json.reads

  implicit val aliasReads: Reads[Alias] = StringReads.flatMapResult { x =>
    val alias =
      if (x.count(_ == ':') == 2) Alias.fromString(x)
      else Alias.createWithChainId(x, AddressScheme.current.chainId)

    alias
      .flatMap { x =>
        Either.cond(x.chainId == AddressScheme.current.chainId, x, TxValidationError.WrongChain(AddressScheme.current.chainId, x.chainId))
      }
      .successOrErrorToString("Alias")
  }

  implicit val addressOrAliasReads: Reads[AddressOrAlias] = byteArrayReads("AddressOrAlias").flatMapResult { x =>
    AddressOrAlias.fromBytes(x).successOrErrorToString("AddressOrAlias")
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

  implicit val leaseInfoReads = DebugApiRoute.leaseInfoFormat

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

  implicit val proofsReads: Reads[Proofs]                   = requests.proofsReads
  implicit val rideRunnerInputReads: Reads[RideRunnerInput] = Json.reads

  def parseByteStr(x: String, hint: String = "ByteStr"): JsResult[ByteStr] = parseByteArray(x, hint).map(ByteStr(_))

  def parseByteArray(x: String, hint: String = "Array[Byte]"): JsResult[Array[Byte]] =
    Try(decodeBytesFromStr(x)).toEither.successOr(e => mkError(hint, e.getMessage))

  def decodeBytesFromStr(x: String): Array[Byte] =
    if (x.startsWith("base58:")) Base58.decode(x.substring(7))
    else if (x.startsWith(Base64.Prefix)) Base64.decode(x)
    else Base58.decode(x)

  def parse(str: String): RideRunnerInput = Json.configured.parse(str).as[RideRunnerInput]

  private implicit final class ValidationErrorOps[E, T](private val self: Either[E, T]) extends AnyVal {
    def successOr(f: E => JsError): JsResult[T]           = self.fold(f, JsSuccess(_))
    def successOrErrorToString(hint: String): JsResult[T] = self.fold(e => mkError(hint, e.toString), JsSuccess(_))
  }

  private def mkError(tpe: String, error: String = "") =
    JsError(Seq(JsPath -> Seq(JsonValidationError(Seq(s"error.expected.$tpe") ++ Seq(error).filterNot(_ == "")))))
}
