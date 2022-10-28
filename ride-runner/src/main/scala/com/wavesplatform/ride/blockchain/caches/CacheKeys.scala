package com.wavesplatform.ride.blockchain.caches

import com.google.common.primitives.{Ints, Longs, Shorts}
import com.google.protobuf.CodedInputStream
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.{
  Key,
  readAccountScriptInfo,
  readAssetDetails,
  readAssetScript,
  readAssetStaticInfo,
  readBlockMeta,
  readLeaseBalance,
  writeAccountScriptInfo,
  writeAssetDetails,
  writeAssetScript,
  writeAssetStaticInfo,
  writeBlockMeta,
  writeLeaseBalance
}
import com.wavesplatform.protobuf.transaction.{PBSignedTransaction, PBTransactions}
import com.wavesplatform.ride.blockchain.caches.AsBytes.{ByteArrayOutputStreamOps, optional}
import com.wavesplatform.serialization.*
import com.wavesplatform.state.{
  AccountScriptInfo,
  AssetDescription,
  AssetInfo,
  AssetStaticInfo,
  AssetVolumeInfo,
  DataEntry,
  Portfolio,
  TransactionId,
  TxMeta
}
import com.wavesplatform.transaction.serialization.impl.DataTxSerializer
import com.wavesplatform.transaction.{Asset, EthereumTransaction, GenesisTransaction, PBSince, PaymentTransaction, Transaction, TransactionParsers}

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import scala.util.chaining.scalaUtilChainingOps

trait AsBytes[T] {
  def toByteArray(x: T): Array[Byte]
  def fromByteArray(xs: Array[Byte]): (T, Int)
}

object AsBytes {
  implicit final class ByteArrayOutputStreamOps(val self: ByteArrayOutputStream) extends AnyVal {
    def writeByte(x: Byte): ByteArrayOutputStream            = self.tap(_.write(x))
    def writeInt(x: Int): ByteArrayOutputStream              = self.tap(_.write(Ints.toByteArray(x)))
    def writeLong(x: Long): ByteArrayOutputStream            = self.tap(_.write(Longs.toByteArray(x)))
    def writeWithLen(xs: Array[Byte]): ByteArrayOutputStream = self.tap(_.writeInt(xs.length).write(xs))
    def writeBool(x: Boolean): ByteArrayOutputStream = {
      val int = if (x) 1 else 0
      self.tap(_.writeByte(int.toByte))
    }
  }

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
          (Some(r), 1 + len)
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
      val bb        = ByteBuffer.wrap(xs)
      val aBytes    = bb.getByteArray(bb.getInt)
      val bBytes    = bb.getByteArray(bb.getInt)
      val (a, aLen) = aAsBytes.fromByteArray(aBytes)
      val (b, bLen) = bAsBytes.fromByteArray(bBytes)
      ((a, b), Ints.BYTES * 2 + aLen + bLen)
    }
  }

  implicit def map[K: AsBytes, V: AsBytes]: AsBytes[Map[K, V]] = new AsBytes[Map[K, V]] {
    private implicit val kvTuple = tuple2[K, V]

    override def toByteArray(x: Map[K, V]): Array[Byte] = {
      val r = new ByteArrayOutputStream().writeInt(x.size)
      x.foreach(kvTuple.toByteArray)
      r.toByteArray
    }

    override def fromByteArray(xs: Array[Byte]): (Map[K, V], Int) = {
      var currXs = xs.drop(Ints.BYTES)
      var len    = Ints.BYTES
      var items  = Ints.fromByteArray(xs)
      var r      = Map.empty[K, V]
      while (items >= 0) {
        val (tuple, tupleLen) = kvTuple.fromByteArray(currXs)
        r += tuple
        len += tupleLen
        items -= 1
        currXs = currXs.drop(tupleLen)
      }
      (r, len)
    }
  }
}

sealed abstract class CacheKey[KeyT, ValueT](prefix: Short, name: String)(implicit keyAsBytes: AsBytes[KeyT], valueAsBytes: AsBytes[ValueT]) {
  def mkKey(key: KeyT): Key[ValueT] = new Key[ValueT](prefix, name, keyAsBytes.toByteArray(key)) {
    override def parse(bytes: Array[Byte]): ValueT = valueAsBytes.fromByteArray(bytes)._1
    override def encode(v: ValueT): Array[Byte]    = valueAsBytes.toByteArray(v)
  }
}

object CacheKeys {
  object Addresses          extends CacheKey[Address, Int](1, "addresses")
  object AccountDataEntries extends CacheKey[(Address, String), Option[DataEntry[?]]](2, "accountDataEntries")
  object AccountScripts     extends CacheKey[Address, Option[AccountScriptInfo]](3, "accountScripts")
  object SignedBlockHeaders extends CacheKey[Int, Option[SignedBlockHeader]](4, "blockHeaders")
  object Height             extends CacheKey[Unit, Int](5, "height")
  object VRF                extends CacheKey[Int, Option[ByteStr]](6, "vrf")
  object ActivatedFeatures  extends CacheKey[Unit, Map[Short, Int]](7, "activatedFeatures")
  object AssetDescriptions  extends CacheKey[Asset.IssuedAsset, Option[AssetDescription]](8, "assetDescriptions")
  object Aliases            extends CacheKey[Alias, Option[Address]](9, "aliases")
  object Portfolios         extends CacheKey[Address, Option[Portfolio]](10, "portfolios")
  object Transactions       extends CacheKey[ByteStr, Option[(TxMeta, Option[Transaction])]](11, "transactions")

  implicit val unitAsBytes: AsBytes[Unit] = new AsBytes[Unit] {
    override def toByteArray(x: Unit): Array[Byte]           = Array.emptyByteArray
    override def fromByteArray(xs: Array[Byte]): (Unit, Int) = ((), 0)
  }

  implicit val shortAsBytes: AsBytes[Short] = new AsBytes[Short] {
    override def toByteArray(x: Short): Array[Byte]           = Shorts.toByteArray(x)
    override def fromByteArray(xs: Array[Byte]): (Short, Int) = (Shorts.fromByteArray(xs), Shorts.BYTES)
  }

  implicit val intAsBytes: AsBytes[Int] = new AsBytes[Int] {
    override def toByteArray(x: Int): Array[Byte]           = Ints.toByteArray(x)
    override def fromByteArray(xs: Array[Byte]): (Int, Int) = (Ints.fromByteArray(xs), Ints.BYTES) // TODO (Int, BytesLen)
  }

  implicit val longAsBytes: AsBytes[Long] = new AsBytes[Long] {
    override def toByteArray(x: Long): Array[Byte]           = Longs.toByteArray(x)
    override def fromByteArray(xs: Array[Byte]): (Long, Int) = (Longs.fromByteArray(xs), Longs.BYTES)
  }

  implicit val utf8StringAsBytes: AsBytes[String] = new AsBytes[String] {
    override def toByteArray(x: String): Array[Byte] =
      new ByteArrayOutputStream()
        .writeWithLen(x.getBytes(StandardCharsets.UTF_8))
        .toByteArray

    override def fromByteArray(xs: Array[Byte]): (String, Int) = {
      val bb  = ByteBuffer.wrap(xs)
      val len = bb.getInt
      (new String(bb.getByteArray(len), StandardCharsets.UTF_8), len)
    }
  }

  implicit val byteStrAsBytes: AsBytes[ByteStr] = new AsBytes[ByteStr] {
    override def toByteArray(x: ByteStr): Array[Byte] = new ByteArrayOutputStream().writeWithLen(x.arr).toByteArray
    override def fromByteArray(xs: Array[Byte]): (ByteStr, Int) = {
      val bb  = ByteBuffer.wrap(xs)
      val len = bb.getInt
      (ByteStr(bb.getByteArray(len)), len)
    }
  }

  implicit val addressAsBytes: AsBytes[Address] = new AsBytes[Address] {
    override def toByteArray(x: Address): Array[Byte]           = x.bytes
    override def fromByteArray(xs: Array[Byte]): (Address, Int) = (Address.fromBytes(xs).explicitGet(), Address.AddressLength)
  }

  implicit val aliasAsBytes: AsBytes[Alias] = new AsBytes[Alias] {
    override def toByteArray(x: Alias): Array[Byte] = new ByteArrayOutputStream().writeWithLen(x.bytes).toByteArray
    override def fromByteArray(xs: Array[Byte]): (Alias, Int) = {
      val bb  = ByteBuffer.wrap(xs)
      val len = bb.getInt
      (Alias.fromBytes(bb.getByteArray(len)).explicitGet(), len)
    }
  }

  // TODO bytestr?
  implicit val issuedAssetAsBytes: AsBytes[Asset.IssuedAsset] = new AsBytes[Asset.IssuedAsset] {
    override def toByteArray(x: Asset.IssuedAsset): Array[Byte] = new ByteArrayOutputStream().writeWithLen(x.id.arr).toByteArray
    override def fromByteArray(xs: Array[Byte]): (Asset.IssuedAsset, Int) = {
      val bb  = ByteBuffer.wrap(xs)
      val len = bb.getInt
      (Asset.IssuedAsset(ByteStr(bb.getByteArray(len))), len)
    }
  }

  implicit val dataEntryAsBytes: AsBytes[DataEntry[_]] = new AsBytes[DataEntry[_]] {
    override def toByteArray(x: DataEntry[_]): Array[Byte] =
      new ByteArrayOutputStream().writeWithLen(DataTxSerializer.serializeEntry(x)).toByteArray

    override def fromByteArray(xs: Array[Byte]): (DataEntry[_], Int) = {
      val bb  = ByteBuffer.wrap(xs)
      val len = bb.getInt
      (DataTxSerializer.parseEntry(bb), len)
    }
  }

  implicit val accountScriptInfoAsBytes: AsBytes[AccountScriptInfo] = new AsBytes[AccountScriptInfo] {
    override def toByteArray(x: AccountScriptInfo): Array[Byte] =
      new ByteArrayOutputStream().writeWithLen(writeAccountScriptInfo(x)).toByteArray

    override def fromByteArray(xs: Array[Byte]): (AccountScriptInfo, Int) = {
      val bb  = ByteBuffer.wrap(xs)
      val len = bb.getInt
      (readAccountScriptInfo(bb.getByteArray(len)), len)
    }
  }

  implicit val blockHeaderAsBytes: AsBytes[SignedBlockHeader] = new AsBytes[SignedBlockHeader] {
    override def toByteArray(x: SignedBlockHeader): Array[Byte] =
      new ByteArrayOutputStream()
        .writeWithLen(
          writeBlockMeta(
            BlockMeta(
              header = x.header,
              signature = x.signature,
              headerHash = None,
              height = 0,
              size = 0,
              transactionCount = 0,
              totalFeeInWaves = 0,
              reward = None,
              vrf = None
            )
          )
        )
        .toByteArray

    override def fromByteArray(xs: Array[Byte]): (SignedBlockHeader, Int) = {
      val bb  = ByteBuffer.wrap(xs)
      val len = bb.getInt
      (readBlockMeta(bb.getByteArray(len)).toSignedHeader, Ints.BYTES + len)
    }
  }

  // staticInfo, assetDetails, sponsorship, assetScript
  implicit val assetDescriptionAsBytes: AsBytes[AssetDescription] = new AsBytes[AssetDescription] {
    override def toByteArray(x: AssetDescription): Array[Byte] = {
      val staticInfo = AssetStaticInfo(TransactionId @@ x.originTransactionId, x.issuer, x.decimals, x.nft)
      val assetInfo  = AssetInfo(x.name, x.description, x.lastUpdatedAt)
      val volumeInfo = AssetVolumeInfo(x.reissuable, x.totalVolume)

      val r = new ByteArrayOutputStream()
        .writeWithLen(writeAssetStaticInfo(staticInfo))
        .writeWithLen(writeAssetDetails((assetInfo, volumeInfo)))
        .writeLong(x.sponsorship)
        .writeBool(x.script.nonEmpty)

      x.script.foreach(x => r.writeWithLen(writeAssetScript(x)))
      r.toByteArray
    }

    override def fromByteArray(xs: Array[Byte]): (AssetDescription, Int) = {
      val bb = ByteBuffer.wrap(xs)

      val staticInfoLen = bb.getInt
      val staticInfo    = readAssetStaticInfo(bb.getByteArray(staticInfoLen))

      val detailsLen              = bb.getInt
      val (assetInfo, volumeInfo) = readAssetDetails(bb.getByteArray(detailsLen))

      val sponsorship = bb.getLong

      // TODO optional
      val (script, scriptLen) = bb.getByte match {
        case 0 => (None, 1)
        case 1 =>
          val (scriptLen, sizeLen) = (bb.getInt, Ints.BYTES)
          val script               = readAssetScript(bb.getByteArray(scriptLen))
          (Some(script), 1 + sizeLen + scriptLen)
        case x => throw new RuntimeException(s"The invalid Option marker: expected 1, but got $x")
      }

      val r = AssetDescription(
        staticInfo.source,
        staticInfo.issuer,
        assetInfo.name,
        assetInfo.description,
        staticInfo.decimals,
        volumeInfo.isReissuable,
        volumeInfo.volume,
        assetInfo.lastUpdatedAt,
        script,
        sponsorship,
        staticInfo.nft
      )
      (r, staticInfoLen + detailsLen + Longs.BYTES + scriptLen)
    }
  }

  implicit val portfoliosAsBytes: AsBytes[Portfolio] = new AsBytes[Portfolio] {
    private val assetsAsBytes: AsBytes[Map[Asset.IssuedAsset, Long]] = AsBytes.map[Asset.IssuedAsset, Long]

    override def toByteArray(x: Portfolio): Array[Byte] =
      new ByteArrayOutputStream()
        .writeLong(x.balance)
        .writeWithLen(writeLeaseBalance(x.lease))
        .writeWithLen(assetsAsBytes.toByteArray(x.assets))
        .toByteArray

    override def fromByteArray(xs: Array[Byte]): (Portfolio, Int) = {
      val bb = ByteBuffer.wrap(xs)

      val balance = bb.getLong

      val leaseLen = bb.getInt
      val lease    = readLeaseBalance(bb.getByteArray(leaseLen)) // TODO AsBytes?

      val assetsLen   = bb.getInt
      val (assets, _) = assetsAsBytes.fromByteArray(bb.getByteArray(assetsLen))

      val portfolio = Portfolio(
        balance = balance,
        lease = lease,
        assets = assets
      )

      (portfolio, Longs.BYTES + leaseLen + assetsLen)
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
        (parse(bb.getByteArray(len)), len)
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

      (tx, 1 + Ints.BYTES + len)
    }
  }
}
