package com.wavesplatform.ride.input

import cats.syntax.either.*
import com.google.protobuf.ByteString
import com.google.protobuf.UnsafeByteOperations.unsafeWrap
import com.wavesplatform.account.{Address, AddressOrAlias, Alias}
import com.wavesplatform.api.http.{DebugApiRoute, requests}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.InvokeScriptResult.DataEntry
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, AssetScriptInfo, BalanceSnapshot, Height, LeaseBalance, TxMeta}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.transfer.{TransferTransaction, TransferTransactionLike}
import com.wavesplatform.transaction.{Asset, Proofs, TransactionFactory, TxPositiveAmount}
import play.api.libs.json.*

import java.util.Locale
import scala.util.Try

// TODO #14: Longs in JS
case class RideRunnerInput(
    address: Address,
    request: JsObject,
    trace: Boolean = false,
    accounts: Map[Address, RunnerAccountState] = Map.empty,
    height: Int = 3296626,
    extraFeatures: Set[Short] = Set.empty,
    assets: Map[IssuedAsset, RunnerAssetInfo] = Map.empty,
    blocks: Map[Int, RunnerBlockInfo] = Map.empty,
    transactions: Map[ByteStr, RunnerTransactionInfo] = Map.empty
) {
  lazy val accountScript: Map[Address, RunnerScriptInfo] = for {
    (addr, state) <- accounts
    scriptInfo    <- state.scriptInfo
  } yield addr -> scriptInfo

  lazy val accountData: Map[Address, Map[String, DataEntry]] = for {
    (addr, state) <- accounts
    data          <- state.data
  } yield addr -> data.map { case (key, entry) => key -> entry.toDataEntry(key) }

  lazy val hasData: Map[Address, Boolean] = accountStateLens(_.data.nonEmpty)

  lazy val balance: Map[Address, Map[Asset, Long]] = accountStateLens(_.balance)

  lazy val balanceSnapshots: Map[Address, Seq[BalanceSnapshot]] = for {
    (addr, state) <- accounts
  } yield {
    val generatingBalance = state.generatingBalance.orElse(state.balance.get(Waves)).getOrElse(0L)
    addr -> Seq(BalanceSnapshot(height, generatingBalance, 0, 0))
  }

  lazy val leaseBalance: Map[Address, LeaseBalance] = for {
    (addr, state) <- accounts
    lease         <- state.leasing
  } yield addr -> LeaseBalance(lease.in, lease.out)

  private def accountStateLens[T](f: RunnerAccountState => T): Map[Address, T] = for {
    (addr, state) <- accounts
  } yield addr -> f(state)

  lazy val blockHeader: Map[Int, SignedBlockHeader] = for {
    (height, blockInfo) <- blocks
  } yield height -> SignedBlockHeader(
    header = BlockHeader(
      version = 5,
      timestamp = blockInfo.timestamp,
      reference = ByteStr(Array.emptyByteArray),
      baseTarget = blockInfo.baseTarget,
      generationSignature = blockInfo.generationSignature,
      generator = blockInfo.generatorPublicKey,
      featureVotes = Nil,
      rewardVote = -1,
      transactionsRoot = ByteStr(Array.emptyByteArray)
    ),
    signature = ByteStr(Array.emptyByteArray)
  )

  lazy val hitSource: Map[Int, ByteStr] = for {
    (height, blockInfo) <- blocks
    vrf                 <- blockInfo.VRF
  } yield height -> vrf
}

object RideRunnerInput {

  implicit val jsonConfiguration = JsonConfiguration[Json.WithDefaultValues](
    discriminator = "type",
    typeNaming = JsonNaming { fullName =>
      fullName.split('.').last.replace("RunnerDataEntry", "").toLowerCase(Locale.US)
    }
  )

  // TODO #14: numericMapFormat
  implicit def shortMapFormat[T: Format]: Format[Map[Short, T]] = mapFormat[Short, T](
    _.toString,
    x => x.toShortOption.fold[JsResult[Short]](JsError(s"Can't parse int: $x"))(JsSuccess(_))
  )

  implicit def intMapFormat[T: Format]: Format[Map[Int, T]] = mapFormat[Int, T](
    _.toString,
    x => x.toIntOption.fold[JsResult[Int]](JsError(s"Can't parse int: $x"))(JsSuccess(_))
  )

  implicit def byteStrMapFormat[T: Format]: Format[Map[ByteStr, T]] = mapFormat[ByteStr, T](
    _.base64,
    x => Try(ByteStr(decodeBytesFromStr(x))).fold[JsResult[ByteStr]](e => JsError(s"Can't parse ByteStr '$x': ${e.getMessage}"), JsSuccess(_))
  )

  implicit def addressMapFormat[T: Format]: Format[Map[Address, T]] = mapFormat[Address, T](
    _.toString,
    x => Address.fromString(x).fold[JsResult[Address]](e => JsError(s"Can't parse Address '$x': $e"), JsSuccess(_))
  )

  implicit def aliasMapFormat[T: Format]: Format[Map[Alias, T]] = mapFormat[Alias, T](
    _.name,
    x => Alias.fromString(x).fold[JsResult[Alias]](e => JsError(s"Can't parse Alias '$x': $e"), JsSuccess(_))
  )

  implicit def assetMapFormat[T: Format]: Format[Map[Asset, T]] = mapFormat[Asset, T](
    _.toString,
    { str =>
      val compatStr = if (str == "WAVES") None else Some(str)
      Try(Asset.fromString(compatStr)).fold[JsResult[Asset]](e => JsError(s"Can't parse Asset '$str': ${e.getMessage}"), JsSuccess(_))
    }
  )

  implicit def issuedAssetMapFormat[T: Format]: Format[Map[IssuedAsset, T]] = mapFormat[IssuedAsset, T](
    _.toString,
    { str =>
      ByteStr
        .decodeBase58(str)
        .map(IssuedAsset(_))
        .fold[JsResult[IssuedAsset]](e => JsError(s"Can't parse IssuedAsset '$str': ${e.getMessage}"), JsSuccess(_))
    }
  )

  implicit def optBlockMapFormat[T: Format]: Format[Map[Option[BlockId], T]] = mapFormat[Option[BlockId], T](
    _.fold("")(_.base64),
    str =>
      if (str.isEmpty) JsSuccess(None)
      else
        Try(decodeBytesFromStr(str))
          .fold[JsResult[BlockId]](e => JsError(s"Can't parse BlockId '$str': ${e.getMessage}"), xs => JsSuccess(ByteStr(xs)))
          .map(Some(_))
  )

  implicit val scriptFormat: Format[Script] = implicitly[Format[String]]
    .bimap(
      Script.fromBase64String(_).explicitGet(), // TODO #14: JsError instead
      _.bytes().base64Raw
    )
  implicit val accountScriptInfoFormat: OFormat[AccountScriptInfo] = Json.format

  implicit val aliasFormat: Format[Alias] = implicitly[Format[String]]
    .bimap(
      Alias.fromString(_).explicitGet(), // TODO #14: JsError instead
      _.name
    )

  implicit val byteStringFormat: Format[ByteString] = implicitly[Format[String]]
    .bimap(
      str => unsafeWrap(decodeBytesFromStr(str)),
      x => s"${Base64.Prefix}${Base64.encode(x.toByteArray)}"
    )

  // from test/http
  implicit val addressOrAliasFormat: Format[AddressOrAlias] = Format[AddressOrAlias](
    Reads {
      case JsString(str) =>
        Base58
          .tryDecodeWithLimit(str)
          .toEither
          .leftMap(_ => ())
          .flatMap(AddressOrAlias.fromBytes(_).leftMap(_ => ()))
          .map(JsSuccess(_))
          .getOrElse(JsError("Can't read Address or Alias"))

      case _ => JsError("Can't read Address or Alias")
    },
    Writes(x => JsString(x.toString))
  )

  // IDEA and the compiler don't know that it is used
  implicit val byteStrFormat = com.wavesplatform.utils.byteStrFormat

  implicit val heightFormat: Format[Height] = Height.lift

  implicit val assetScriptInfoFormat: OFormat[AssetScriptInfo] = Json.format

  implicit val assetDescriptionFormat: OFormat[AssetDescription] = Json.format

  implicit val blockHeaderFormat: OFormat[BlockHeader]             = Json.format
  implicit val signedBlockHeaderFormat: OFormat[SignedBlockHeader] = Json.format

  implicit val txMetaFormat: OFormat[TxMeta] = Json.format

  implicit val transferTransactionLikeFormat: OFormat[TransferTransactionLike] = OFormat(
    r = Reads { js =>
      TransactionFactory.fromSignedRequest(js) match {
        case Left(e) => JsError(s"Can't parse a transaction: $e")
        case Right(tx) =>
          tx match {
            case tx: TransferTransactionLike => JsSuccess(tx)
            case _                           => JsError(s"Expected ${tx.id} to be a transfer-like transaction, but got: type=${tx.tpe}")
          }
      }
    },
    w = OWrites {
      case tx: TransferTransaction => tx.json()
      case tx                      => throw new RuntimeException(s"Impossible case: $tx")
    }
  )

  implicit val leaseInfoFormat = DebugApiRoute.leaseInfoFormat

  implicit val balanceSnapshotFormat: OFormat[BalanceSnapshot]       = Json.format
  implicit val runnerLeaseBalanceFormat: OFormat[RunnerLeaseBalance] = Json.format

  implicit val runnerScriptInfoFormat: OFormat[RunnerScriptInfo] = Json.format

  implicit val binaryRunnerDataEntryFormat: OFormat[BinaryRunnerDataEntry]   = Json.format
  implicit val booleanRunnerDataEntryFormat: OFormat[BooleanRunnerDataEntry] = Json.format
  implicit val integerRunnerDataEntryFormat: OFormat[IntegerRunnerDataEntry] = Json.format
  implicit val stringRunnerDataEntryFormat: OFormat[StringRunnerDataEntry]   = Json.format

  implicit val runnerDataEntryFormat: OFormat[RunnerDataEntry] = Json.format

  implicit val runnerAccountStateFormat: OFormat[RunnerAccountState] = Json.format

  implicit val runnerAssetInfoFormat: OFormat[RunnerAssetInfo] = Json.format

  implicit val runnerBlockInfoFormat: OFormat[RunnerBlockInfo] = Json.format

  implicit val runnerTransactionInfoFormat: OFormat[RunnerTransactionInfo] = Json.format

  implicit val stdLibVersionFormat: Format[StdLibVersion] = implicitly[Format[Int]]
    .bimap(StdLibVersion.VersionDic.idMap.apply, _.id)

  implicit val txPositiveAmountFormat: Format[TxPositiveAmount] = implicitly[Format[Long]]
    .bimap(TxPositiveAmount.from(_).explicitGet(), _.value)

  implicit val proofsReads: Reads[Proofs]                    = requests.proofsReads
  implicit val rideRunnerInputFormat: Reads[RideRunnerInput] = Json.reads

  def mapFormat[K, V: Format](stringifyKey: K => String, parseKey: String => JsResult[K])(implicit vFormat: Format[V]): Format[Map[K, V]] = {
    Format(
      fjs = Reads {
        case JsObject(xs) =>
          xs.foldLeft[JsResult[Map[K, V]]](JsSuccess(Map.empty[K, V])) { case (r, (k, v)) =>
            for {
              r <- r
              k <- parseKey(k)
              v <- vFormat.reads(v)
            } yield r.updated(k, v)
          }
        case x => JsError(s"Can't parse map: $x")
      },
      tjs = Writes.map[V](vFormat).contramap(_.map { case (k, v) => stringifyKey(k) -> v })
    )
  }

  def decodeBytesFromStr(str: String): Array[Byte] =
    if (str.startsWith("base58:")) Base58.decode(str.substring(7))
    else if (str.startsWith(Base64.Prefix)) Base64.decode(str)
    else Base58.decode(str)

  def parse(str: String): RideRunnerInput             = Json.configured.parse(str).as[RideRunnerInput]
  def parseMany(str: String): Vector[RideRunnerInput] = Json.configured.parse(str).as[Vector[RideRunnerInput]]
}
