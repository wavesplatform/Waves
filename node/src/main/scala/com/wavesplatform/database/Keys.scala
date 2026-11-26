package com.wavesplatform.database

import com.google.common.primitives.{Ints, Longs}

import java.nio.ByteBuffer
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.crypto.bls.BlsPublicKey
import com.wavesplatform.database.protobuf.{EthereumTransactionMeta, StaticAssetInfo, TransactionMeta, BlockMeta as PBBlockMeta}
import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot
import com.wavesplatform.state.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{ERC20Address, Transaction}
import com.wavesplatform.utils.*

case class CurrentBalance(balance: Long, height: Height, prevHeight: Height)
object CurrentBalance {
  val Unavailable: CurrentBalance = CurrentBalance(0L, Height(0), Height(0))
}

case class BalanceNode(balance: Long, prevHeight: Height)
object BalanceNode {
  val Empty: BalanceNode = BalanceNode(0, Height(0))
  val SizeInBytes: Int   = 12
}

case class CurrentVolumeAndFee(volume: Long, fee: Long, height: Height, prevHeight: Height)
object CurrentVolumeAndFee {
  val Unavailable: CurrentVolumeAndFee = CurrentVolumeAndFee(0, 0, Height(0), Height(0))
}

case class VolumeAndFeeNode(volume: Long, fee: Long, prevHeight: Height)
object VolumeAndFeeNode {
  val Empty: VolumeAndFeeNode = VolumeAndFeeNode(0, 0, Height(0))
}

case class CurrentLeaseBalance(in: Long, out: Long, height: Height, prevHeight: Height)
object CurrentLeaseBalance {
  val Unavailable: CurrentLeaseBalance = CurrentLeaseBalance(0, 0, Height(0), Height(0))
}

case class LeaseBalanceNode(in: Long, out: Long, prevHeight: Height)
object LeaseBalanceNode {
  val Empty: LeaseBalanceNode = LeaseBalanceNode(0, 0, Height(0))
}

// The height of the previous entry is kept only in the history node, which rollback and cleanup read
case class CurrentData(entry: DataEntry[?], height: Height)
object CurrentData {
  def empty(key: String): CurrentData = CurrentData(EmptyDataEntry(key), Height(0))
}

case class DataNode(entry: DataEntry[?], prevHeight: Height)
object DataNode {
  def empty(key: String): DataNode = DataNode(EmptyDataEntry(key), Height(0))
}

private[database] case class HistoryNode[A](prevHeight: Height, value: A)

private[database] case class LinkedHistory[A](head: Key[Height], at: Height => Key[Option[HistoryNode[A]]]) {
  def read(height: Height, get: Key[Option[HistoryNode[A]]] => Option[HistoryNode[A]]): Option[A] =
    Option.when(height > Height(0))(height).flatMap(h => get(at(h))).map(_.value)
}

object Keys {
  import KeyHelpers.*
  import KeyTag.{
    AddressId as AddressIdTag,
    EthereumTransactionMeta as EthereumTransactionMetaTag,
    InvokeScriptResult as InvokeScriptResultTag,
    LeaseDetails as LeaseDetailsTag,
    Height as HeightTag,
    *
  }

  val version: Key[Int]   = intKey(Version, default = 1)
  val height: Key[Height] = heightKey(HeightTag)

  def heightOf(blockId: ByteStr): Key[Option[Int]] = Key.opt[Int](HeightOf, blockId.arr, Ints.fromByteArray, Ints.toByteArray)

  def wavesBalance(addressId: AddressId): Key[CurrentBalance] =
    Key(WavesBalance, addressId.toByteArray, readCurrentBalance, writeCurrentBalance)

  def wavesBalanceAt(addressId: AddressId, height: Height, cfh: RDB.ByHeightHandle): Key[BalanceNode] =
    byHeight(WavesBalanceHistory, height, readBalanceNode, writeBalanceNode, cfh, addressId.toByteArray)

  def assetBalance(addressId: AddressId, asset: IssuedAsset): Key[CurrentBalance] =
    Key(AssetBalance, addressId.toByteArray ++ asset.id.arr, readCurrentBalance, writeCurrentBalance)

  def assetBalanceHolderPrefix(asset: IssuedAsset): Array[Byte] = AssetBalanceHolder.prefixBytes ++ asset.id.arr

  def assetBalanceHolder(addressId: AddressId, asset: IssuedAsset): Key[Unit] =
    Key(AssetBalanceHolder, asset.id.arr ++ addressId.toByteArray, _ => (), _ => Array.emptyByteArray)

  def assetBalanceAt(addressId: AddressId, asset: IssuedAsset, height: Height, cfh: RDB.ByHeightHandle): Key[BalanceNode] =
    byHeight(AssetBalanceHistory, height, readBalanceNode, writeBalanceNode, cfh, asset.id.arr ++ addressId.toByteArray)

  private[database] def assetDetails(asset: IssuedAsset, cfh: RDB.ByHeightHandle): LinkedHistory[(AssetInfo, AssetVolumeInfo)] =
    linkedHistory(AssetDetailsHead, AssetDetails, asset.id.arr, readAssetDetails, writeAssetDetails, cfh)

  def issuedAssets(height: Height, cfh: RDB.ByHeightHandle): Key[Seq[IssuedAsset]] =
    byHeight(IssuedAssets, height, d => readAssetIds(d).map(IssuedAsset(_)), ias => writeAssetIds(ias.map(_.id)), cfh)
  def updatedAssets(height: Height, cfh: RDB.ByHeightHandle): Key[Seq[IssuedAsset]] =
    byHeight(UpdatedAssets, height, d => readAssetIds(d).map(IssuedAsset(_)), ias => writeAssetIds(ias.map(_.id)), cfh)
  def sponsorshipAssets(height: Height, cfh: RDB.ByHeightHandle): Key[Seq[IssuedAsset]] =
    byHeight(SponsoredAssets, height, d => readAssetIds(d).map(IssuedAsset(_)), ias => writeAssetIds(ias.map(_.id)), cfh)
  def leaseBalanceAt(addressId: AddressId, height: Height, cfh: RDB.ByHeightHandle): Key[LeaseBalanceNode] =
    byHeight(LeaseBalanceHistory, height, readLeaseBalanceNode, writeLeaseBalanceNode, cfh, addressId.toByteArray)

  def leaseBalance(addressId: AddressId): Key[CurrentLeaseBalance] =
    Key(LeaseBalance, addressId.toByteArray, readLeaseBalance, writeLeaseBalance)

  def leaseDetailsHistory(leaseId: ByteStr, cfh: RDB.ByHeightHandle): Key[Seq[Height]] =
    historyIndex(LeaseDetailsHistory, leaseId.arr, cfh)
  def leaseDetails(leaseId: ByteStr, cfh: RDB.ByHeightHandle)(height: Height): Key[Option[LeaseDetails]] =
    Key.opt(LeaseDetailsTag, h(height) ++ leaseId.arr, readLeaseDetails, writeLeaseDetails, Some(cfh.handle), heightFirst = true)

  def filledVolumeAndFeeAt(orderId: ByteStr, height: Height, cfh: RDB.ByHeightHandle): Key[VolumeAndFeeNode] =
    byHeight(FilledVolumeAndFeeHistory, height, readVolumeAndFeeNode, writeVolumeAndFeeNode, cfh, orderId.arr)

  def filledVolumeAndFee(orderId: ByteStr): Key[CurrentVolumeAndFee] =
    Key(FilledVolumeAndFee, orderId.arr, readVolumeAndFee, writeVolumeAndFee)

  def changedAddresses(height: Height): Key[Seq[AddressId]] = Key(ChangedAddresses, h(height), readAddressIds, writeAddressIds)

  def changedWavesBalances(height: Height): Key[Seq[AddressId]] =
    Key(ChangedWavesBalances, h(height), readAddressIds, writeAddressIds)

  def changedBalances(height: Height, asset: IssuedAsset): Key[Seq[AddressId]] =
    Key(ChangedAssetBalances, h(height) ++ asset.id.arr, readAddressIds, writeAddressIds)

  def changedBalancesAtPrefix(height: Height): Array[Byte] = KeyTag.ChangedAssetBalances.prefixBytes ++ h(height)

  def addressIdOfAlias(alias: Alias): Key[Option[AddressId]] = Key.opt(AddressIdOfAlias, alias.bytes, AddressId.fromByteArray, _.toByteArray)

  val lastAddressId: Key[Option[Long]] = Key.opt(LastAddressId, Array.emptyByteArray, Longs.fromByteArray, _.toByteArray)

  def addressId(address: Address): Key[Option[AddressId]] = Key.opt(AddressIdTag, address.bytes, AddressId.fromByteArray, _.toByteArray)
  def idToAddress(addressId: AddressId): Key[Address]     = Key(IdToAddress, addressId.toByteArray, Address.fromBytes(_).explicitGet(), _.bytes)

  private[database] def addressScript(addressId: AddressId, cfh: RDB.ByHeightHandle): LinkedHistory[Option[AccountScriptInfo]] =
    linkedHistory(
      AddressScriptHead,
      AddressScript,
      addressId.toByteArray,
      readAddressScriptVersion,
      _.fold(Array.emptyByteArray)(writeAccountScriptInfo),
      cfh
    )

  private[database] def readAddressScriptVersion(payload: Array[Byte]): Option[AccountScriptInfo] =
    Option.when(payload.nonEmpty)(readAccountScriptInfo(payload))

  def addressScriptPresent(addressId: AddressId, height: Height, cfh: RDB.ByHeightHandle): Key[Boolean] =
    scriptPresent(AddressScript, height, addressId.toByteArray, cfh)

  val approvedFeatures: Key[Map[Short, Height]]  = Key(ApprovedFeatures, Array.emptyByteArray, readFeatureMap, writeFeatureMap)
  val activatedFeatures: Key[Map[Short, Height]] = Key(ActivatedFeatures, Array.emptyByteArray, readFeatureMap, writeFeatureMap)

  def data(addressId: AddressId, key: String): Key[CurrentData] =
    Key(Data, addressId.toByteArray ++ key.utf8Bytes, readCurrentData(key), writeCurrentData)

  // Height first: the nodes of one height are the keys changed at it, so rollback and cleanup iterate them by prefix
  def dataAt(addressId: AddressId, key: String, cfh: RDB.DataHistoryHandle)(height: Height): Key[DataNode] =
    Key(DataHistory, h(height) ++ addressId.toByteArray ++ key.utf8Bytes, readDataNode(key), writeDataNode, Some(cfh.handle))

  def dataHistoryAtPrefix(height: Height): Array[Byte] = KeyTag.DataHistory.prefixBytes ++ h(height)

  private[database] def sponsorship(asset: IssuedAsset, cfh: RDB.ByHeightHandle): LinkedHistory[SponsorshipValue] =
    linkedHistory(SponsorshipHead, Sponsorship, asset.id.arr, readSponsorship, writeSponsorship, cfh)

  def carryFee(height: Height, cfh: RDB.ByHeightHandle): Key[Long] =
    byHeight(CarryFee, height, Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray, cfh)

  private[database] def assetScript(asset: IssuedAsset, cfh: RDB.ByHeightHandle): LinkedHistory[AssetScriptInfo] =
    linkedHistory(AssetScriptHead, AssetScript, asset.id.arr, readAssetScript, writeAssetScript, cfh)

  def assetScriptPresent(asset: IssuedAsset, height: Height, cfh: RDB.ByHeightHandle): Key[Boolean] =
    scriptPresent(AssetScript, height, asset.id.arr, cfh)

  val safeRollbackHeight: Key[Height] = heightKey(SafeRollbackHeight)
  val lastCleanupHeight: Key[Height]  = heightKey(LastCleanupHeight)

  def blockMetaAt(height: Height, cfh: RDB.BlockMetaHandle): Key[Option[PBBlockMeta]] =
    Key.opt(BlockInfoAtHeight, h(height), readBlockMeta, writeBlockMeta, Some(cfh.handle))

  def blockInfoBytesAt(height: Height, cfh: RDB.BlockMetaHandle): Key[Option[Array[Byte]]] =
    Key.opt(
      BlockInfoAtHeight,
      h(height),
      identity,
      unsupported("Can not explicitly write block bytes"),
      Some(cfh.handle)
    )

  def transactionAt(height: Height, n: TxNum, cfHandle: RDB.TxHandle): Key[Option[(TxMeta, Transaction)]] =
    Key.opt[(TxMeta, Transaction)](
      NthTransactionInfoAtHeight,
      hNum(height, n),
      readTransaction(height),
      writeTransaction,
      Some(cfHandle.handle)
    )

  def transactionStateSnapshotAt(height: Height, n: TxNum, cfHandle: RDB.TxSnapshotHandle): Key[Option[TransactionStateSnapshot]] =
    Key.opt[TransactionStateSnapshot](
      NthTransactionStateSnapshotAtHeight,
      hNum(height, n),
      TransactionStateSnapshot.parseFrom,
      _.toByteArray,
      Some(cfHandle.handle)
    )

  def addressTransactionSeqNr(addressId: AddressId, cfh: RDB.ApiHandle): Key[Int] =
    bytesSeqNr(AddressTransactionSeqNr, addressId.toByteArray, cfh = Some(cfh.handle))

  def addressTransactionHN(addressId: AddressId, seqNr: Int, cfh: RDB.ApiHandle): Key[Option[(Height, Seq[(Byte, TxNum, Int)])]] =
    Key.opt(
      AddressTransactionHeightTypeAndNums,
      intBytes(addressId.toByteArray, seqNr),
      readTransactionHNSeqAndType,
      writeTransactionHNSeqAndType,
      Some(cfh.handle)
    )

  def addressLeaseSeqNr(addressId: AddressId, cfh: RDB.ApiHandle): Key[Int] =
    bytesSeqNr(AddressLeaseInfoSeqNr, addressId.toByteArray, cfh = Some(cfh.handle))

  def addressLeaseSeq(addressId: AddressId, seqNr: Int, cfh: RDB.ApiHandle): Key[Option[Seq[ByteStr]]] =
    Key.opt(
      AddressLeaseInfoSeq,
      intBytes(addressId.toByteArray, seqNr),
      readLeaseIdSeq,
      writeLeaseIdSeq,
      Some(cfh.handle)
    )

  def transactionMetaById(txId: TransactionId, cfh: RDB.TxMetaHandle): Key[Option[TransactionMeta]] =
    Key.opt(
      TransactionMetaById,
      txId.arr,
      TransactionMeta.parseFrom,
      _.toByteArray,
      Some(cfh.handle)
    )

  def invokeScriptResult(height: Height, txNum: TxNum, cfh: RDB.InvokeResultsHandle): Key[Option[InvokeScriptResult]] =
    Key.opt(InvokeScriptResultTag, hNum(height, txNum), InvokeScriptResult.fromBytes, InvokeScriptResult.toBytes, Some(cfh.handle))

  val disabledAliases: Key[Set[Alias]] = Key(
    DisabledAliases,
    Array.emptyByteArray,
    b => readStrings(b).map(s => Alias.create(s).explicitGet()).toSet,
    as => writeStrings(as.map(_.name).toSeq)
  )

  def assetStaticInfo(asset: IssuedAsset): Key[Option[StaticAssetInfo]] =
    Key.opt(AssetStaticInfo, asset.id.arr.take(20), StaticAssetInfo.parseFrom, _.toByteArray)

  def assetStaticInfo(addr: ERC20Address): Key[Option[StaticAssetInfo]] =
    Key.opt(AssetStaticInfo, addr.arr, StaticAssetInfo.parseFrom, _.toByteArray)

  def nftCount(addressId: AddressId, cfh: RDB.ApiHandle): Key[Int] =
    Key(NftCount, addressId.toByteArray, Option(_).fold(0)(Ints.fromByteArray), Ints.toByteArray, Some(cfh.handle))

  def nftAt(addressId: AddressId, index: Int, assetId: IssuedAsset, cfh: RDB.ApiHandle): Key[Option[Unit]] =
    Key.opt(NftPossession, addressId.toByteArray ++ Longs.toByteArray(index) ++ assetId.id.arr, _ => (), _ => Array.emptyByteArray, Some(cfh.handle))

  def stateHash(height: Height, cfh: RDB.ByHeightHandle): Key[Option[StateHash]] =
    Key.opt(StateHash, h(height), readStateHash, writeStateHash, Some(cfh.handle), heightFirst = true)

  def blockStateHash(height: Height, cfh: RDB.ByHeightHandle): Key[ByteStr] =
    byHeight(BlockStateHash, height, Option(_).fold(TxStateSnapshotHashBuilder.InitStateHash)(ByteStr(_)), _.arr, cfh)

  def ethereumTransactionMeta(height: Height, txNum: TxNum, cfh: RDB.ApiHandle): Key[Option[EthereumTransactionMeta]] =
    Key.opt(EthereumTransactionMetaTag, hNum(height, txNum), EthereumTransactionMeta.parseFrom, _.toByteArray, Some(cfh.handle))

  def maliciousMinerBanHeights(addressBytes: Array[Byte], cfh: RDB.ByHeightHandle): Key[Seq[Height]] =
    historyIndex(MaliciousMinerBanHeights, addressBytes, cfh)

  // Writes only after DeterministicFinality activation
  val finalizedHeight: Key[Option[Height]] = Key(
    FinalizedBlockHeight,
    keySuffix = Array.emptyByteArray,
    readFinalizedHeight,
    _.fold(Array.emptyByteArray)(_.toByteArray)
  )

  def finalizedHeightAt(at: Height, cfh: RDB.ByHeightHandle): Key[Option[Height]] =
    byHeight(FinalizedBlockHeightAt, at, readFinalizedHeight, _.fold(Array.emptyByteArray)(_.toByteArray), cfh)

  // Predecessors become unreachable once their successors are below the rollback cutoff.
  private[database] def linkedHistoryPrune(height: Height): Key[Seq[Array[Byte]]] =
    Key(
      LinkedHistoryPrune,
      h(height),
      bytes =>
        Option(bytes).fold(Seq.empty[Array[Byte]]) { data =>
          val in   = ByteBuffer.wrap(data)
          val keys = Vector.newBuilder[Array[Byte]]
          while (in.hasRemaining) {
            val key = new Array[Byte](in.get() & 0xff)
            in.get(key)
            keys += key
          }
          keys.result()
        },
      keys => {
        val out = ByteBuffer.allocate(keys.iterator.map(_.length + 1).sum)
        keys.foreach { key =>
          require(key.length <= 255)
          out.put(key.length.toByte).put(key)
        }
        out.array()
      }
    )

  private def linkedHistory[A](
      headTag: KeyTag,
      versionTag: KeyTag,
      suffix: Array[Byte],
      read: Array[Byte] => A,
      write: A => Array[Byte],
      cfh: RDB.ByHeightHandle
  ): LinkedHistory[A] = {
    val head = Key(headTag, suffix, bytes => Height(Option(bytes).fold(0)(Ints.fromByteArray)), h)
    val at = (height: Height) =>
      Key.opt(
        versionTag,
        h(height) ++ suffix,
        bytes => HistoryNode(Height(Ints.fromByteArray(bytes)), read(bytes.drop(Ints.BYTES))),
        node => h(node.prevHeight) ++ write(node.value),
        Some(cfh.handle),
        heightFirst = true
      )
    LinkedHistory(head, at)
  }

  private def scriptPresent(tag: KeyTag, height: Height, suffix: Array[Byte], cfh: RDB.ByHeightHandle): Key[Boolean] =
    byHeight(tag, height, bytes => bytes != null && bytes.length > Ints.BYTES, unsupported("Can not write script presence"), cfh, suffix)

  // Per-entity history indexes have no version height. Zero keeps them outside the block-height ranges.
  private def historyIndex(tag: KeyTag, suffix: Array[Byte], cfh: RDB.ByHeightHandle): Key[Seq[Height]] =
    byHeight(tag, Height(0), bs => Height.seq(readIntSeq(bs)*), hs => writeIntSeq(Height.ints(hs)), cfh, suffix)

  private def byHeight[V](
      tag: KeyTag,
      height: Height,
      parser: Array[Byte] => V,
      encoder: V => Array[Byte],
      cfh: RDB.ByHeightHandle,
      suffix: Array[Byte] = Array.emptyByteArray
  ): Key[V] =
    Key(tag, h(height) ++ suffix, parser, encoder, Some(cfh.handle), heightFirst = true)

  def heightTagPrefix(height: Height, tag: KeyTag): Array[Byte] = h(height) ++ tag.prefixBytes

  private def readFinalizedHeight(bytes: Array[Byte]): Option[Height] =
    Option(bytes).collect { case bs if bs.length == Ints.BYTES => com.wavesplatform.state.Height(Ints.fromByteArray(bytes)) }

  /** Key: Int(committedPeriod.start) ++ Int(commitmentHeight)
    * @note
    *   committedPeriod.start >= commitmentHeight, because a generator can commit only for a next period
    */
  def committedGenerators(committedPeriod: GenerationPeriod, commitmentHeight: Height): Key[Option[Seq[(AddressId, BlsPublicKey)]]] =
    Key.opt(
      CommittedGenerators,
      h(committedPeriod.start) ++ h(commitmentHeight),
      readCommittedGenerators,
      writeCommittedGenerators
    )

  def conflictGenerators(committedPeriod: GenerationPeriod, conflictEndorsementHeight: Height): Key[Seq[GeneratorIndex]] =
    Key(
      ConflictGenerators,
      h(committedPeriod.start) ++ h(conflictEndorsementHeight),
      readConflictGenerators,
      writeConflictGenerators
    )

  def commitmentTransactions(committedPeriod: GenerationPeriod, commitmentHeight: Height): Key[Seq[TransactionId]] =
    Key(
      CommitmentTransactions,
      h(committedPeriod.start) ++ h(commitmentHeight),
      readCommitmentTransactions,
      writeCommitmentTransactions
    )

  def generatorBalances(at: Height, cfh: RDB.ApiHandle): Key[Option[Seq[(GeneratorIndex, Long)]]] =
    Key.opt(GeneratorBalances, h(at), readGeneratorBalances, writeGeneratorBalances, Some(cfh.handle))
}
