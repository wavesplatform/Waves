package com.wavesplatform.ride.runner.storage.persistent

import com.google.common.primitives.{Ints, Longs, Shorts}
import com.google.protobuf.{CodedInputStream, UnsafeByteOperations}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.blockchain.SignedBlockHeaderWithVrf
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.protobuf.{StaticAssetInfo, BlockMeta as PBBlockMeta}
import com.wavesplatform.database.rocksdb.{
  AddressId,
  Key,
  readAccountScriptInfo,
  readAssetDetails,
  readAssetScript,
  readBlockMeta,
  writeAccountScriptInfo,
  writeAssetDetails,
  writeAssetScript,
  writeBlockMeta
}
import com.wavesplatform.meta.getSimpleName
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.protobuf.transaction.{PBSignedTransaction, PBTransactions}
import com.wavesplatform.ride.runner.storage.persistent.AsBytes.ByteArrayOutputStreamOps
import com.wavesplatform.ride.runner.storage.{DbKeyIndex, ScriptRequest}
import com.wavesplatform.serialization.*
import com.wavesplatform.state
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, AssetInfo, AssetVolumeInfo, DataEntry, LeaseBalance, TransactionId, TxMeta}
import com.wavesplatform.transaction.serialization.impl.DataTxSerializer
import com.wavesplatform.transaction.{Asset, EthereumTransaction, GenesisTransaction, PBSince, PaymentTransaction, Transaction, TransactionParsers}
import play.api.libs.json.{JsObject, Json}

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import scala.collection.compat.immutable.ArraySeq
import scala.reflect.ClassTag
import scala.util.chaining.scalaUtilChainingOps

trait AsBytes[T] {
  def toByteArray(x: T): Array[Byte]

  def fromByteArray(xs: Array[Byte]): (T, Int)
}

object AsBytes {
  def apply[T](implicit r: AsBytes[T]): AsBytes[T] = r

  implicit final class AsBytesOps[A](val self: AsBytes[A]) extends AnyVal {
    def transform[B](toB: A => B, fromB: B => A): AsBytes[B] = new AsBytes[B] {
      override def toByteArray(x: B): Array[Byte] = self.toByteArray(fromB(x))

      override def fromByteArray(xs: Array[Byte]): (B, Int) = {
        val (a, len) = self.fromByteArray(xs)
        (toB(a), len)
      }
    }
  }

  implicit final class ByteArrayOutputStreamOps(val self: ByteArrayOutputStream) extends AnyVal {
    def writeByte(x: Byte): ByteArrayOutputStream = self.tap(_.write(x))

    def writeInt(x: Int): ByteArrayOutputStream = self.tap(_.write(Ints.toByteArray(x)))

    def writeLong(x: Long): ByteArrayOutputStream = self.tap(_.write(Longs.toByteArray(x)))

    def writeWithLen(xs: Array[Byte]): ByteArrayOutputStream = self.tap(_.writeInt(xs.length).write(xs))

    def writeBool(x: Boolean): ByteArrayOutputStream = {
      val int = if (x) 1 else 0
      self.tap(_.writeByte(int.toByte))
    }
  }

  implicit val unitAsBytes: AsBytes[Unit] = new AsBytes[Unit] {
    override def toByteArray(x: Unit): Array[Byte] = Array.emptyByteArray

    override def fromByteArray(xs: Array[Byte]): (Unit, Int) = ((), 0)
  }

  implicit val shortAsBytes: AsBytes[Short] = new AsBytes[Short] {
    override def toByteArray(x: Short): Array[Byte] = Shorts.toByteArray(x)

    override def fromByteArray(xs: Array[Byte]): (Short, Int) = (Shorts.fromByteArray(xs), Shorts.BYTES)
  }

  implicit val intAsBytes: AsBytes[Int] = new AsBytes[Int] {
    override def toByteArray(x: Int): Array[Byte] = Ints.toByteArray(x)

    override def fromByteArray(xs: Array[Byte]): (Int, Int) = (Ints.fromByteArray(xs), Ints.BYTES) // TODO #23 (Int, BytesLen)
  }

  implicit val longAsBytes: AsBytes[Long] = new AsBytes[Long] {
    override def toByteArray(x: Long): Array[Byte] = Longs.toByteArray(x)

    override def fromByteArray(xs: Array[Byte]): (Long, Int) = (Longs.fromByteArray(xs), Longs.BYTES)
  }

  implicit val byteArrayAsBytes: AsBytes[Array[Byte]] = new AsBytes[Array[Byte]] {
    override def toByteArray(xs: Array[Byte]): Array[Byte] = new ByteArrayOutputStream().writeWithLen(xs).toByteArray

    override def fromByteArray(xs: Array[Byte]): (Array[Byte], Int) = {
      val bb  = ByteBuffer.wrap(xs)
      val len = bb.getInt
      (bb.getByteArray(len), Ints.BYTES + len)
    }
  }

  implicit val utf8StringAsBytes: AsBytes[String] = byteArrayAsBytes.transform(
    new String(_, StandardCharsets.UTF_8),
    _.getBytes(StandardCharsets.UTF_8)
  )

  implicit def optional[T](implicit underlying: AsBytes[T]): AsBytes[Option[T]] = new AsBytes[Option[T]] {
    override def toByteArray(x: Option[T]): Array[Byte] = x match {
      case None => Array[Byte](0)
      case Some(x) =>
        new ByteArrayOutputStream()
          .writeByte(1)
          .writeWithLen(underlying.toByteArray(x))
          .toByteArray
    }

    override def fromByteArray(xs: Array[Byte]): (Option[T], Int) = {
      val bb = ByteBuffer.wrap(xs)
      bb.getByte match {
        case 0 => (None, 1)
        case 1 =>
          val (r, len) = underlying.fromByteArray(bb.getByteArray(bb.getInt))
          (Some(r), 1 + Ints.BYTES + len)
        case x => throw new RuntimeException(s"The invalid Option marker: expected 1, but got $x")
      }
    }
  }

  implicit def tuple2[A, B](implicit aAsBytes: AsBytes[A], bAsBytes: AsBytes[B]): AsBytes[(A, B)] = new AsBytes[(A, B)] {
    override def toByteArray(x: (A, B)): Array[Byte] = {
      val (a, b) = x
      new ByteArrayOutputStream()
        .writeWithLen(aAsBytes.toByteArray(a))
        .writeWithLen(bAsBytes.toByteArray(b))
        .toByteArray
    }

    override def fromByteArray(xs: Array[Byte]): ((A, B), Int) = {
      val bb = ByteBuffer.wrap(xs)

      val aBytes    = bb.getByteArray(bb.getInt)
      val (a, aLen) = aAsBytes.fromByteArray(aBytes)

      val bBytes    = bb.getByteArray(bb.getInt)
      val (b, bLen) = bAsBytes.fromByteArray(bBytes)

      ((a, b), Ints.BYTES + aLen + Ints.BYTES + bLen)
    }
  }

  implicit def tuple3[A, B, C](implicit aAsBytes: AsBytes[A], bAsBytes: AsBytes[B], cAsBytes: AsBytes[C]): AsBytes[(A, B, C)] =
    new AsBytes[(A, B, C)] {
      override def toByteArray(x: (A, B, C)): Array[Byte] = {
        val (a, b, c) = x
        new ByteArrayOutputStream()
          .writeWithLen(aAsBytes.toByteArray(a))
          .writeWithLen(bAsBytes.toByteArray(b))
          .writeWithLen(cAsBytes.toByteArray(c))
          .toByteArray
      }

      override def fromByteArray(xs: Array[Byte]): ((A, B, C), Int) = {
        val bb = ByteBuffer.wrap(xs)

        val aBytes    = bb.getByteArray(bb.getInt)
        val (a, aLen) = aAsBytes.fromByteArray(aBytes)

        val bBytes    = bb.getByteArray(bb.getInt)
        val (b, bLen) = bAsBytes.fromByteArray(bBytes)

        val cBytes    = bb.getByteArray(bb.getInt)
        val (c, cLen) = cAsBytes.fromByteArray(cBytes)

        ((a, b, c), Ints.BYTES + aLen + Ints.BYTES + bLen + Ints.BYTES + cLen)
      }
    }

  implicit def seq[V: ClassTag](implicit vAsBytes: AsBytes[V]): AsBytes[Seq[V]] = new AsBytes[Seq[V]] {
    override def toByteArray(xs: Seq[V]): Array[Byte] = {
      val r = new ByteArrayOutputStream().writeInt(xs.size)
      xs.foreach { x => r.writeWithLen(vAsBytes.toByteArray(x)) }
      r.toByteArray
    }

    override def fromByteArray(xs: Array[Byte]): (Seq[V], Int) = {
      val bb = ByteBuffer.wrap(xs)

      val len       = bb.getInt
      var readBytes = Ints.BYTES

      val r = new Array[V](len)
      var i = 0
      while (i < len) {
        val (x, xLen) = vAsBytes.fromByteArray(bb.getByteArray(bb.getInt))
        r(i) = x
        readBytes += Ints.BYTES + xLen
        i += 1
      }

      (ArraySeq.unsafeWrapArray(r), readBytes)
    }
  }

  implicit def map[K: AsBytes, V: AsBytes]: AsBytes[Map[K, V]] = new AsBytes[Map[K, V]] {
    private implicit val kvTuple = tuple2[K, V]

    override def toByteArray(xs: Map[K, V]): Array[Byte] = {
      val r = new ByteArrayOutputStream().writeInt(xs.size)
      xs.foreach { x => r.writeWithLen(kvTuple.toByteArray(x)) }
      r.toByteArray
    }

    override def fromByteArray(xs: Array[Byte]): (Map[K, V], Int) = {
      val bb = ByteBuffer.wrap(xs)

      var restItems = bb.getInt
      var len       = Ints.BYTES

      var r = Map.empty[K, V]
      while (restItems > 0) {
        val (tuple, tupleLen) = kvTuple.fromByteArray(bb.getByteArray(bb.getInt))
        r += tuple
        len += Ints.BYTES + tupleLen
        restItems -= 1
      }

      (r, len)
    }
  }
}

sealed abstract class CacheKey[KeyT, ValueT](prefix: Short)(implicit keyAsBytes: AsBytes[KeyT], valueAsBytes: AsBytes[ValueT]) {
  val name             = getSimpleName(this)
  val prefixWithOffset = (CacheKey.prefixOffset + prefix).toShort
  val prefixBytes      = Shorts.toByteArray(prefixWithOffset)

  def mkKey(key: KeyT): Key[ValueT] = new Key[ValueT](prefixWithOffset, name, keyAsBytes.toByteArray(key)) {
    override def parse(bytes: Array[Byte]): ValueT = valueAsBytes.fromByteArray(bytes)._1

    override def encode(v: ValueT): Array[Byte] = valueAsBytes.toByteArray(v)
  }

  def parseKey(xs: Array[Byte]): KeyT = keyAsBytes.fromByteArray(xs.drop(Shorts.BYTES))._1

  def parseValue(xs: Array[Byte]): ValueT = valueAsBytes.fromByteArray(xs)._1
}

object CacheKey {
  private val prefixOffset = 150 // To not interfere with NODE's DB
}

sealed abstract class CacheHistoryKey[KeyT: AsBytes](prefix: Short) extends CacheKey[KeyT, Seq[Int]](prefix)

object CacheKeys {
  object LastAddressId extends CacheKey[Unit, AddressId](0)
  object AddressToId   extends CacheKey[Address, AddressId](1)
  object IdToAddress   extends CacheKey[AddressId, Address](2)

  // TODO stats: how often keys are changed?
  object AccountDataEntriesHistory extends CacheHistoryKey[(AddressId, String)](11)
  object AccountDataEntries        extends CacheKey[(AddressId, String, Int), Option[DataEntry[?]]](12)

  object AccountScriptsHistory extends CacheHistoryKey[AddressId](21)
  object AccountScripts        extends CacheKey[(AddressId, Int), Option[AccountScriptInfo]](22)

  object SignedBlockHeadersWithVrf extends CacheKey[Int, SignedBlockHeaderWithVrf](30)

  object Height extends CacheKey[Unit, Int](40) {
    val Key = mkKey(())
  }

  object VRF extends CacheKey[Int, Option[ByteStr]](50)

  object ActivatedFeatures extends CacheKey[Unit, Map[Short, Int]](60)

  object AssetDescriptionsHistory extends CacheHistoryKey[Asset.IssuedAsset](71)
  object AssetDescriptions        extends CacheKey[(Asset.IssuedAsset, Int), Option[AssetDescription]](72)

  // TODO #25 Store AddressId
  object Aliases extends CacheKey[Alias, Option[Address]](80)

  object AccountAssetsHistory extends CacheHistoryKey[(AddressId, Asset)](91)
  object AccountAssets        extends CacheKey[(AddressId, Asset, Int), Long](92)

  object AccountLeaseBalancesHistory extends CacheHistoryKey[AddressId](101)
  object AccountLeaseBalances        extends CacheKey[(AddressId, Int), LeaseBalance](102)

  object Transactions extends CacheKey[TransactionId, Option[Int]](110)

  object RequestsLastIndex extends CacheKey[Unit, Int](121)
  object Requests          extends CacheKey[Int, ScriptRequest](122)

  implicit val jsObjectAsBytes: AsBytes[JsObject] = AsBytes[String].transform(
    s =>
      Json.parse(s) match {
        case r: JsObject => r
        case r           => throw new RuntimeException(s"Expected JsObject, got $r")
      },
    Json.stringify(_)
  )

  implicit val byteStrAsBytes: AsBytes[ByteStr] = AsBytes[Array[Byte]].transform(ByteStr(_), _.arr)

  implicit val transactionIdAsBytes: AsBytes[TransactionId] = byteStrAsBytes.transform(TransactionId(_), x => x)

  implicit val dbKeyIndex: AsBytes[DbKeyIndex] = AsBytes.intAsBytes.transform(DbKeyIndex(_), x => x)

  implicit val addressId: AsBytes[AddressId] = new AsBytes[AddressId] {
    override def toByteArray(x: AddressId): Array[Byte] = x.toByteArray

    override def fromByteArray(xs: Array[Byte]): (AddressId, Int) = (AddressId.fromByteArray(xs), Longs.BYTES)
  }

  implicit val addressAsBytes: AsBytes[Address] = new AsBytes[Address] {
    override def toByteArray(x: Address): Array[Byte] = x.bytes

    override def fromByteArray(xs: Array[Byte]): (Address, Int) = (Address.fromBytes(xs).explicitGet(), Address.AddressLength)
  }

  implicit val aliasAsBytes: AsBytes[Alias] = AsBytes[Array[Byte]].transform(Alias.fromBytes(_).explicitGet(), _.bytes)

  implicit val issuedAssetAsBytes: AsBytes[Asset.IssuedAsset] = AsBytes[ByteStr].transform(Asset.IssuedAsset(_), _.id)

  implicit val assetAsBytes: AsBytes[Asset] = AsBytes[Option[ByteStr]].transform(Asset.fromCompatId, _.compatId)

  implicit val leaseBalanceAsBytes: AsBytes[LeaseBalance] = AsBytes[(Long, Long)].transform(Function.tupled(LeaseBalance.apply), x => (x.in, x.out))

  implicit val dataEntryAsBytes: AsBytes[DataEntry[?]] = new AsBytes[DataEntry[?]] {
    override def toByteArray(x: DataEntry[?]): Array[Byte] =
      new ByteArrayOutputStream().writeWithLen(DataTxSerializer.serializeEntry(x)).toByteArray

    override def fromByteArray(xs: Array[Byte]): (DataEntry[?], Int) = {
      val bb  = ByteBuffer.wrap(xs)
      val len = bb.getInt
      (DataTxSerializer.parseEntry(bb), Ints.BYTES + len)
    }
  }

  implicit val accountScriptInfoAsBytes: AsBytes[AccountScriptInfo] = AsBytes[Array[Byte]].transform(readAccountScriptInfo, writeAccountScriptInfo)

  implicit val blockHeaderAsBytes: AsBytes[SignedBlockHeaderWithVrf] = new AsBytes[SignedBlockHeaderWithVrf] {
    override def toByteArray(x: SignedBlockHeaderWithVrf): Array[Byte] =
      new ByteArrayOutputStream()
        .writeWithLen(
          writeBlockMeta(
            PBBlockMeta(
              header = Some(PBBlocks.protobuf(x.header.header)),
              signature = UnsafeByteOperations.unsafeWrap(x.header.signature.arr),
              vrf = UnsafeByteOperations.unsafeWrap(x.vrf.arr)
            )
          )
        )
        .toByteArray

    override def fromByteArray(xs: Array[Byte]): (SignedBlockHeaderWithVrf, Int) = {
      val bb  = ByteBuffer.wrap(xs)
      val len = bb.getInt
      (toSignedHeader(readBlockMeta(bb.getByteArray(len))), Ints.BYTES + len)
    }

    private def toSignedHeader(m: PBBlockMeta): SignedBlockHeaderWithVrf =
      SignedBlockHeaderWithVrf(
        SignedBlockHeader(PBBlocks.vanilla(m.getHeader), m.signature.toByteStr),
        m.vrf.toByteStr
      )
  }

  implicit val assetDescriptionAsBytes: AsBytes[AssetDescription] = new AsBytes[AssetDescription] {
    override def toByteArray(x: AssetDescription): Array[Byte] = {
      // TODO id here is empty, now it is used to optimize reads in NODE for Blockchain.resolveERC20Address
      val staticInfo = StaticAssetInfo(
        sourceId = UnsafeByteOperations.unsafeWrap(x.originTransactionId.arr),
        issuerPublicKey = UnsafeByteOperations.unsafeWrap(x.issuer.arr),
        decimals = x.decimals,
        isNft = x.nft
      )
      val assetInfo  = AssetInfo(x.name, x.description, x.lastUpdatedAt)
      val volumeInfo = AssetVolumeInfo(x.reissuable, x.totalVolume)

      val r = new ByteArrayOutputStream()
        .writeWithLen(StaticAssetInfo.toByteArray(staticInfo))
        .writeWithLen(writeAssetDetails((assetInfo, volumeInfo)))
        .writeLong(x.sponsorship)
        .writeBool(x.script.nonEmpty)

      x.script.foreach(x => r.writeWithLen(writeAssetScript(x)))
      r.toByteArray
    }

    override def fromByteArray(xs: Array[Byte]): (AssetDescription, Int) = {
      val bb = ByteBuffer.wrap(xs)

      val staticInfoLen = bb.getInt
      val staticInfo    = StaticAssetInfo.parseFrom(bb.getByteArray(staticInfoLen))

      val detailsLen              = bb.getInt
      val (assetInfo, volumeInfo) = readAssetDetails(bb.getByteArray(detailsLen))

      val sponsorship = bb.getLong

      val (script, scriptLen) = bb.getByte match {
        case 0 => (None, 1)
        case 1 =>
          val scriptLen = bb.getInt
          val script    = readAssetScript(bb.getByteArray(scriptLen))
          (Some(script), 1 + Ints.BYTES + scriptLen)
        case x => throw new RuntimeException(s"The invalid Option marker: expected 1, but got $x")
      }

      val r = AssetDescription(
        staticInfo.sourceId.toByteStr,
        staticInfo.issuerPublicKey.toPublicKey,
        assetInfo.name,
        assetInfo.description,
        staticInfo.decimals,
        volumeInfo.isReissuable,
        volumeInfo.volume,
        assetInfo.lastUpdatedAt,
        script,
        sponsorship,
        staticInfo.isNft,
        staticInfo.sequenceInBlock,
        state.Height @@ staticInfo.height
      )
      (r, Ints.BYTES + staticInfoLen + Ints.BYTES + detailsLen + Longs.BYTES + scriptLen)
    }
  }

  implicit val txMetaAsBytes: AsBytes[TxMeta] = new AsBytes[TxMeta] {
    override def toByteArray(x: TxMeta): Array[Byte] =
      new ByteArrayOutputStream()
        .writeInt(x.height)
        .writeBool(x.succeeded)
        .writeLong(x.spentComplexity)
        .toByteArray

    override def fromByteArray(xs: Array[Byte]): (TxMeta, Int) = {
      val bb = ByteBuffer.wrap(xs)
      (TxMeta(com.wavesplatform.state.Height(bb.getInt), bb.getBoolean, bb.getLong), Ints.BYTES + 1 + Longs.BYTES)
    }
  }

  implicit val transferTransactionLikeAsBytes: AsBytes[Transaction] = new AsBytes[Transaction] {
    override def toByteArray(tx: Transaction): Array[Byte] = {
      val r = new ByteArrayOutputStream()
      tx match {
        case lps: PBSince if !lps.isProtobufVersion => r.writeByte(1.toByte).writeWithLen(tx.bytes())
        case _: GenesisTransaction                  => r.writeByte(1.toByte).writeWithLen(tx.bytes())
        case _: PaymentTransaction                  => r.writeByte(1.toByte).writeWithLen(tx.bytes())
        case tx: EthereumTransaction                => r.writeByte(2.toByte).writeWithLen(tx.bytes())
        case _                                      => r.writeByte(3.toByte).writeWithLen(PBTransactions.protobuf(tx).toByteArray)
      }
      r.toByteArray
    }

    override def fromByteArray(xs: Array[Byte]): (Transaction, Int) = {
      val bb = ByteBuffer.wrap(xs)

      def parse(parse: Array[Byte] => Transaction): (Transaction, Int) = {
        val len = bb.getInt
        (parse(bb.getByteArray(len)), Ints.BYTES + len)
      }

      val (tx, len) = bb.getByte match {
        case 1 => parse(TransactionParsers.parseBytes(_).get) // legacy
        case 2 => parse(EthereumTransaction(_).explicitGet())
        case 3 =>
          parse { xs =>
            PBTransactions
              .vanilla(PBSignedTransaction.parseFrom(CodedInputStream.newInstance(xs)), unsafe = false)
              .explicitGet()
          }
        case x => throw new IllegalArgumentException(s"Illegal transaction data: $x")
      }

      (tx, 1 + len)
    }
  }

  implicit val requestKeyAsBytes: AsBytes[ScriptRequest] = AsBytes[(Address, JsObject)].transform(
    Function.tupled(ScriptRequest.apply),
    x => (x.address, x.requestBody)
  )
}
