package com.wavesplatform.ride

import com.google.protobuf.ByteString
import com.google.protobuf.UnsafeByteOperations.unsafeWrap
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.http.DebugApiRoute
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.ride.input.{RunnerAccountState, RunnerBlockInfo, RunnerTransactionInfo}
import com.wavesplatform.state.InvokeScriptResult.DataEntry
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, AssetScriptInfo, BalanceSnapshot, Height, LeaseBalance, TxMeta}
import com.wavesplatform.transaction.transfer.{TransferTransaction, TransferTransactionLike}
import com.wavesplatform.transaction.{Asset, TransactionFactory}
import play.api.libs.json.{Json as PJson, *}

import scala.util.Try

case class RideRunnerInput(
    scriptAddress: Address,
    trace: Boolean,
    request: JsObject,
    accounts: Map[Address, RunnerAccountState] = Map.empty,
    height: Int,
    activatedFeatures: Map[Short, Int] = Map.empty,
    resolveAlias: Map[Alias, Address] = Map.empty,
    assetDescription: Map[Asset, AssetDescription] = Map.empty,
    blocks: Map[Int, RunnerBlockInfo] = Map.empty,
    transactions: Map[ByteStr, RunnerTransactionInfo] = Map.empty
) {
  lazy val accountScript: Map[Address, AccountScriptInfo] = for {
    (addr, state) <- accounts
    script        <- state.scriptInfo
  } yield addr -> script

  lazy val accountData: Map[Address, Map[String, DataEntry]] = accountStateLens(_.data)

  lazy val hasData: Map[Address, Boolean] = accountStateLens(state => state.hasData.getOrElse(state.data.nonEmpty))

  lazy val balance: Map[Address, Map[Asset, Long]] = accountStateLens(_.balance)

  lazy val balanceSnapshots: Map[Address, Map[Int, Map[Option[BlockId], Seq[BalanceSnapshot]]]] = accountStateLens(_.balanceHistory)

  lazy val leaseBalance: Map[Address, LeaseBalance] = for {
    (addr, state) <- accounts
    lease         <- state.leasing
  } yield addr -> lease

  private def accountStateLens[T](f: RunnerAccountState => T): Map[Address, T] = for {
    (addr, state) <- accounts
  } yield addr -> f(state)

  lazy val blockHeader: Map[Int, SignedBlockHeader] = for {
    (height, blockInfo) <- blocks
  } yield height -> blockInfo.blockHeader

  lazy val hitSource: Map[Int, ByteStr] = for {
    (height, blockInfo) <- blocks
    vrf                 <- blockInfo.VRF
  } yield height -> vrf

  lazy val transactionMeta: Map[ByteStr, TxMeta] = for {
    (id, transactionInfo) <- transactions
    meta                  <- transactionInfo.meta
  } yield id -> meta

  lazy val transferById: Map[ByteStr, TransferTransactionLike] = for {
    (id, transactionInfo) <- transactions
    transaction           <- transactionInfo.transaction
  } yield id -> transaction
}

object RideRunnerInput {

  val Json = PJson.using[PJson.WithDefaultValues]

  // TODO numericMapFormat
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
      Script.fromBase64String(_).explicitGet(), // TODO JsError instead
      _.bytes().base64Raw
    )
  implicit val accountScriptInfoFormat: OFormat[AccountScriptInfo] = Json.format

  implicit val aliasFormat: Format[Alias] = implicitly[Format[String]]
    .bimap(
      Alias.fromString(_).explicitGet(), // TODO JsError instead
      _.name
    )

  implicit val byteStringFormat: Format[ByteString] = implicitly[Format[String]]
    .bimap(
      str => unsafeWrap(decodeBytesFromStr(str)),
      x => s"${Base64.Prefix}${Base64.encode(x.toByteArray)}"
    )

  // IDEA and the compiler don't know that it is used
  implicit val byteStrFormat = com.wavesplatform.api.http.requests.byteStrFormat

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

  implicit val balanceSnapshotFormat: OFormat[BalanceSnapshot] = Json.format

  implicit val runnerAccountStateFormat: OFormat[RunnerAccountState] = Json.format

  implicit val runnerBlockInfoFormat: OFormat[RunnerBlockInfo] = Json.format

  implicit val runnerTransactionInfoFormat: OFormat[RunnerTransactionInfo] = Json.format

  implicit val rideRunnerInputFormat: OFormat[RideRunnerInput] = Json.format

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
}
