package com.wavesplatform.ride.runner.storage.persistent

import cats.syntax.option.*
import com.fasterxml.jackson.databind.util.ByteBufferBackedInputStream
import com.google.common.primitives.Shorts
import com.google.protobuf.UnsafeByteOperations
import com.wavesplatform.account.PublicKeys.EmptyPublicKey
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.blockchain.SignedBlockHeaderWithVrf
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.protobuf.{StaticAssetInfo, BlockMeta as PBBlockMeta}
import com.wavesplatform.database.rocksdb.{
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
import com.wavesplatform.database.{AddressId, toPbTransaction, toVanillaTransaction, protobuf as pb}
import com.wavesplatform.meta.getSimpleName
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.protobuf.{ByteStrExt, ByteStringExt}
import com.wavesplatform.ride.runner.db.Heights
import com.wavesplatform.ride.runner.storage.persistent.AsBytes.*
import com.wavesplatform.ride.runner.storage.persistent.syntax.*
import com.wavesplatform.ride.runner.storage.{DbKeyIndex, WeighedAccountScriptInfo, WeighedAssetDescription}
import com.wavesplatform.state
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, AssetInfo, AssetVolumeInfo, DataEntry, LeaseBalance, TransactionId, TxMeta}
import com.wavesplatform.transaction.serialization.impl.DataTxSerializer
import com.wavesplatform.transaction.{Asset, AssetIdLength, Transaction}
import org.rocksdb.ColumnFamilyHandle

import java.io.{ByteArrayOutputStream, OutputStream}
import java.nio.ByteBuffer

sealed abstract class KvPair[KeyT, ValueT](
    prefix: Short,
    val columnFamilyHandle: Option[ColumnFamilyHandle] = None
)(implicit keyAsBytes: AsBytes[KeyT], valueAsBytes: AsBytes[ValueT]) {
  val name        = getSimpleName(this)
  val prefixBytes = Shorts.toByteArray(prefix)

  val prefixedKeyAsBytes: AsBytes[KeyT] = keyAsBytes.prefixed(prefixBytes)

  def at(key: KeyT): Key[ValueT] = {
    val keyBytesStream = new ByteArrayOutputStream()
    keyAsBytes.write(keyBytesStream, key)
    new Key[ValueT](prefix, name, keyBytesStream.toByteArray, columnFamilyHandle) {
      override def parse(bytes: Array[Byte]): ValueT = valueAsBytes.read(bytes)
      override def encode(v: ValueT): Array[Byte]    = valueAsBytes.asBytes(v)
    }
  }

  def parseKey(xs: Array[Byte]): KeyT     = prefixedKeyAsBytes.read(ByteBuffer.wrap(xs))
  def parseValue(xs: Array[Byte]): ValueT = valueAsBytes.read(ByteBuffer.wrap(xs))
}

object KvPair {
  val PrefixSize = Shorts.BYTES
}

sealed abstract class KvHistoryPair[KeyT](prefix: Short)(implicit keyAsBytes: AsBytes[KeyT])
    extends KvPair[KeyT, Heights](prefix)(keyAsBytes, vecAsBytes.consumeAll)

object KvPairs {
  object LastAddressId extends KvPair[Unit, AddressId](0)
  object AddressToId   extends KvPair[Address, AddressId](1)
  object IdToAddress   extends KvPair[AddressId, Address](2)

  // TODO stats: how often keys are changed?
  object AccountDataEntriesHistory extends KvHistoryPair[(AddressId, String)](11)(tuple(implicitly, utf8StringAsBytes.consumeAll))
  object AccountDataEntries
      extends KvPair[(state.Height, (AddressId, String)), Option[DataEntry[?]]](12)(
        tuple(implicitly, tuple(implicitly, utf8StringAsBytes.consumeAll)),
        implicitly
      )

  object AccountScriptsHistory extends KvHistoryPair[AddressId](21)

  val accountScriptInfoAsBytes: AsBytes[AccountScriptInfo] =
    AsBytes.byteArrayAsBytes.consumeAll.transform(readAccountScriptInfo, writeAccountScriptInfo)

  val weighedAccountScriptInfoAsBytes: AsBytes[WeighedAccountScriptInfo] =
    AsBytes
      .tuple2(intAsBytes, accountScriptInfoAsBytes)
      .transform(
        { case (weight, x) => WeighedAccountScriptInfo(weight, x.script, x.verifierComplexity, x.complexitiesByEstimator) },
        x => (x.scriptInfoWeight, AccountScriptInfo(EmptyPublicKey, x.script, x.verifierComplexity, x.complexitiesByEstimator))
      )

  object AccountScripts
      extends KvPair[(state.Height, AddressId), Option[WeighedAccountScriptInfo]](22)(implicitly, AsBytes.optional(weighedAccountScriptInfoAsBytes))

  val blockHeaderAsBytes: AsBytes[SignedBlockHeaderWithVrf] =
    AsBytes.byteArrayAsBytes.consumeAll.transform(
      xs => {
        val x = readBlockMeta(xs)
        SignedBlockHeaderWithVrf(
          SignedBlockHeader(PBBlocks.vanilla(x.getHeader), x.signature.toByteStr),
          x.vrf.toByteStr,
          x.reward
        )
      },
      x =>
        writeBlockMeta(
          PBBlockMeta(
            header = Some(PBBlocks.protobuf(x.header.header)),
            signature = UnsafeByteOperations.unsafeWrap(x.header.signature.arr),
            vrf = UnsafeByteOperations.unsafeWrap(x.vrf.arr),
            reward = x.blockReward
          )
        )
    )

  object SignedBlockHeadersWithVrf extends KvPair[state.Height, SignedBlockHeaderWithVrf](30)(implicitly, blockHeaderAsBytes)

  object Height extends KvPair[Unit, state.Height](40) {
    val Key = at(())
  }

  object ActivatedFeatures extends KvPair[Unit, Map[Short, state.Height]](60)(implicitly, mapAsBytes.consumeAll)

  object AssetDescriptionsHistory extends KvHistoryPair[Asset.IssuedAsset](71)

  val assetDescriptionAsBytes: AsBytes[AssetDescription] = new AsBytes[AssetDescription] {
    override def read(from: ByteBuffer): AssetDescription = {
      val staticInfo              = StaticAssetInfo.parseFrom(from.readWithShortLen())
      val (assetInfo, volumeInfo) = readAssetDetails(from.readWithShortLen())
      val sponsorship             = from.readLong()

      val script = from.readOpt(readAssetScript(from.readWithIntLen()))
      AssetDescription(
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
    }

    override def write(output: OutputStream, x: AssetDescription): Unit =
      output
        .writeWithShortLen(
          StaticAssetInfo.toByteArray(
            // id is empty, now it is used to optimize reads in NODE for Blockchain.resolveERC20Address.
            // we don't need it, because we have it in a key.
            StaticAssetInfo(
              sourceId = x.originTransactionId.toByteString,
              issuerPublicKey = x.issuer.toByteString,
              decimals = x.decimals,
              isNft = x.nft
            )
          )
        )
        .writeWithShortLen(
          writeAssetDetails(
            (
              AssetInfo(x.name, x.description, x.lastUpdatedAt),
              AssetVolumeInfo(x.reissuable, x.totalVolume)
            )
          )
        )
        .writeLong(x.sponsorship)
        .writeBool(x.script.nonEmpty)
        .writeOpt(x.script.map(writeAssetScript))
  }

  val weighedAssetDescriptionAsBytes: AsBytes[WeighedAssetDescription] =
    AsBytes.tuple2(intAsBytes, assetDescriptionAsBytes).transform(Function.tupled(WeighedAssetDescription), x => (x.scriptWeight, x.assetDescription))
  object AssetDescriptions
      extends KvPair[(state.Height, Asset.IssuedAsset), Option[WeighedAssetDescription]](72)(
        implicitly,
        AsBytes.optional(weighedAssetDescriptionAsBytes)
      )

  val aliasAsBytes: AsBytes[Alias]                = AsBytes.byteArrayAsBytes.consumeAll.transform(Alias.fromBytes(_).explicitGet(), _.bytes)
  private val aliasWithLenAsBytes: AsBytes[Alias] = AsBytes.byteArrayAsBytes.withIntLen.transform(Alias.fromBytes(_).explicitGet(), _.bytes)
  object AliasesByHeight extends KvPair[state.Height, List[Alias]](79)(implicitly, AsBytes.listAsBytes.consumeAll(aliasWithLenAsBytes))
  object Aliases         extends KvPair[Alias, (state.Height, Option[AddressId])](80)(aliasAsBytes, implicitly)

  object AccountAssetsHistory extends KvHistoryPair[(AddressId, Asset)](91)
  object AccountAssets        extends KvPair[(state.Height, (AddressId, Asset)), Long](92)

  object AccountLeaseBalancesHistory extends KvHistoryPair[AddressId](101)
  object AccountLeaseBalances        extends KvPair[(state.Height, AddressId), LeaseBalance](102)

  implicit val transactionIdAsBytes: AsBytes[TransactionId]       = AsBytes.byteArrayAsBytes.consumeAll.toByteStr.transform(TransactionId(_), x => x)
  private val transactionIdWithLenAsBytes: AsBytes[TransactionId] = AsBytes.byteArrayAsBytes.withIntLen.toByteStr.transform(TransactionId(_), x => x)
  object TransactionsByHeight
      extends KvPair[state.Height, List[TransactionId]](109)(implicitly, AsBytes.listAsBytes.consumeAll(transactionIdWithLenAsBytes))
  object Transactions extends KvPair[TransactionId, Option[state.Height]](110)

  implicit val dbKeyIndex: AsBytes[DbKeyIndex] = AsBytes.intAsBytes.transform(DbKeyIndex(_), x => x)

  implicit val addressId: AsBytes[AddressId] = AsBytes.longAsBytes.transform(AddressId(_), x => x)

  implicit val addressAsBytes: AsBytes[Address] =
    AsBytes.byteArrayAsBytes.fixed(Address.AddressLength).transform[Address](Address.fromBytes(_).explicitGet(), _.bytes)

  implicit val issuedAssetAsBytes: AsBytes[Asset.IssuedAsset] =
    AsBytes.byteArrayAsBytes.fixed(AssetIdLength).toByteStr.transform(Asset.IssuedAsset(_), _.id)

  implicit val assetAsBytes: AsBytes[Asset] =
    AsBytes[Option[Asset.IssuedAsset]].transform(_.getOrElse(Asset.Waves), x => x.fold(none[Asset.IssuedAsset])(_.some))

  implicit val leaseBalanceAsBytes: AsBytes[LeaseBalance] = AsBytes[(Long, Long)].transform(Function.tupled(LeaseBalance.apply), x => (x.in, x.out))

  implicit val dataEntryAsBytes: AsBytes[DataEntry[?]] =
    AsBytes.mk[DataEntry[?]]((os, x) => os.write(DataTxSerializer.serializeEntry(x)), DataTxSerializer.parseEntry)

  implicit val txMetaAsBytes: AsBytes[TxMeta] = AsBytes[(state.Height, Boolean, Long)].transform[TxMeta](
    Function.tupled(TxMeta.apply),
    x => (x.height, x.succeeded, x.spentComplexity)
  )

  implicit val transferTransactionLikeAsBytes: AsBytes[Transaction] = AsBytes.mk[Transaction](
    (os, tx) => os.write(pb.TransactionData(toPbTransaction(tx)).toByteArray),
    x => toVanillaTransaction(pb.TransactionData.parseFrom(new ByteBufferBackedInputStream(x)).transaction)
  )
}
