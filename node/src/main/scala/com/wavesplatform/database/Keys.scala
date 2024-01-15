package com.wavesplatform.database

import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
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

case class CurrentData(entry: DataEntry[?], height: Height, prevHeight: Height)
object CurrentData {
  def empty(key: String): CurrentData = CurrentData(EmptyDataEntry(key), Height(0), Height(0))
}

case class DataNode(entry: DataEntry[?], prevHeight: Height)
object DataNode {
  def empty(key: String): DataNode = DataNode(EmptyDataEntry(key), Height(0))
}

object Keys {
  import KeyHelpers.*
  import KeyTags.{AddressId as AddressIdTag, EthereumTransactionMeta as EthereumTransactionMetaTag, InvokeScriptResult as InvokeScriptResultTag, LeaseDetails as LeaseDetailsTag, *}

  val version: Key[Int]   = intKey(Version, default = 1)
  val height: Key[Height] = heightKey(Height)

  def heightOf(blockId: ByteStr): Key[Option[Int]] = Key.opt[Int](HeightOf, blockId.arr, Ints.fromByteArray, Ints.toByteArray)

  def wavesBalance(addressId: AddressId): Key[CurrentBalance] =
    Key(WavesBalance, addressId.toByteArray, readCurrentBalance, writeCurrentBalance)

  def wavesBalanceAt(addressId: AddressId, height: Height): Key[BalanceNode] =
    Key(WavesBalanceHistory, hBytes(addressId.toByteArray, height), readBalanceNode, writeBalanceNode)

  def assetBalance(addressId: AddressId, asset: IssuedAsset): Key[CurrentBalance] =
    Key(AssetBalance, addressId.toByteArray ++ asset.id.arr, readCurrentBalance, writeCurrentBalance)

  def assetBalanceAt(addressId: AddressId, asset: IssuedAsset, height: Height): Key[BalanceNode] =
    Key(AssetBalanceHistory, hBytes(asset.id.arr ++ addressId.toByteArray, height), readBalanceNode, writeBalanceNode)

  def assetDetailsHistory(asset: IssuedAsset): Key[Seq[Int]] = historyKey(AssetDetailsHistory, asset.id.arr)
  def assetDetails(asset: IssuedAsset)(height: Int): Key[(AssetInfo, AssetVolumeInfo)] =
    Key(AssetDetails, hBytes(asset.id.arr, height), readAssetDetails, writeAssetDetails)

  def issuedAssets(height: Int): Key[Seq[IssuedAsset]] =
    Key(IssuedAssets, h(height), d => readAssetIds(d).map(IssuedAsset(_)), ias => writeAssetIds(ias.map(_.id)))
  def updatedAssets(height: Int): Key[Seq[IssuedAsset]] =
    Key(UpdatedAssets, h(height), d => readAssetIds(d).map(IssuedAsset(_)), ias => writeAssetIds(ias.map(_.id)))
  def sponsorshipAssets(height: Int): Key[Seq[IssuedAsset]] =
    Key(SponsoredAssets, h(height), d => readAssetIds(d).map(IssuedAsset(_)), ias => writeAssetIds(ias.map(_.id)))
  def leaseBalanceAt(addressId: AddressId, height: Height): Key[LeaseBalanceNode] =
    Key(LeaseBalanceHistory, hBytes(addressId.toByteArray, height), readLeaseBalanceNode, writeLeaseBalanceNode)

  def leaseBalance(addressId: AddressId): Key[CurrentLeaseBalance] =
    Key(LeaseBalance, addressId.toByteArray, readLeaseBalance, writeLeaseBalance)

  def leaseDetailsHistory(leaseId: ByteStr): Key[Seq[Int]] = historyKey(LeaseDetailsHistory, leaseId.arr)
  def leaseDetails(leaseId: ByteStr)(height: Int): Key[Option[LeaseDetails]] =
    Key.opt(LeaseDetailsTag, Ints.toByteArray(height) ++ leaseId.arr, readLeaseDetails, writeLeaseDetails)

  def filledVolumeAndFeeAt(orderId: ByteStr, height: Height): Key[VolumeAndFeeNode] =
    Key(FilledVolumeAndFeeHistory, hBytes(orderId.arr, height), readVolumeAndFeeNode, writeVolumeAndFeeNode)

  def filledVolumeAndFee(orderId: ByteStr): Key[CurrentVolumeAndFee] =
    Key(FilledVolumeAndFee, orderId.arr, readVolumeAndFee, writeVolumeAndFee)

  def changedAddresses(height: Int): Key[Seq[AddressId]] = Key(ChangedAddresses, h(height), readAddressIds, writeAddressIds)

  def changedWavesBalances(height: Int): Key[Seq[AddressId]] =
    Key(ChangedWavesBalances, h(height), readAddressIds, writeAddressIds)

  def changedBalances(height: Int, asset: IssuedAsset): Key[Seq[AddressId]] =
    Key(ChangedAssetBalances, h(height) ++ asset.id.arr, readAddressIds, writeAddressIds)

  def changedBalancesAtPrefix(height: Int): Array[Byte] = KeyTags.ChangedAssetBalances.prefixBytes ++ h(height)

  def addressIdOfAlias(alias: Alias): Key[Option[AddressId]] = Key.opt(AddressIdOfAlias, alias.bytes, AddressId.fromByteArray, _.toByteArray)

  val lastAddressId: Key[Option[Long]] = Key.opt(LastAddressId, Array.emptyByteArray, Longs.fromByteArray, _.toByteArray)

  def addressId(address: Address): Key[Option[AddressId]] = Key.opt(AddressIdTag, address.bytes, AddressId.fromByteArray, _.toByteArray)
  def idToAddress(addressId: AddressId): Key[Address]     = Key(IdToAddress, addressId.toByteArray, Address.fromBytes(_).explicitGet(), _.bytes)

  def addressScriptHistory(addressId: AddressId): Key[Seq[Int]] = historyKey(AddressScriptHistory, addressId.toByteArray)
  def addressScript(addressId: AddressId)(height: Int): Key[Option[AccountScriptInfo]] =
    Key.opt(AddressScript, hAddr(height, addressId), readAccountScriptInfo, writeAccountScriptInfo)

  val approvedFeatures: Key[Map[Short, Int]]  = Key(ApprovedFeatures, Array.emptyByteArray, readFeatureMap, writeFeatureMap)
  val activatedFeatures: Key[Map[Short, Int]] = Key(ActivatedFeatures, Array.emptyByteArray, readFeatureMap, writeFeatureMap)

  def data(addressId: AddressId, key: String): Key[CurrentData] =
    Key(Data, addressId.toByteArray ++ key.utf8Bytes, readCurrentData(key), writeCurrentData)

  def dataAt(addressId: AddressId, key: String)(height: Int): Key[DataNode] =
    Key(DataHistory, hBytes(addressId.toByteArray ++ key.utf8Bytes, height), readDataNode(key), writeDataNode)

  def sponsorshipHistory(asset: IssuedAsset): Key[Seq[Int]] = historyKey(SponsorshipHistory, asset.id.arr)
  def sponsorship(asset: IssuedAsset)(height: Int): Key[SponsorshipValue] =
    Key(Sponsorship, hBytes(asset.id.arr, height), readSponsorship, writeSponsorship)

  def carryFee(height: Int): Key[Long] = Key(CarryFee, h(height), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  def assetScriptHistory(asset: IssuedAsset): Key[Seq[Int]] = historyKey(AssetScriptHistory, asset.id.arr)
  def assetScript(asset: IssuedAsset)(height: Int): Key[Option[AssetScriptInfo]] =
    Key.opt(AssetScript, hBytes(asset.id.arr, height), readAssetScript, writeAssetScript)
  def assetScriptPresent(asset: IssuedAsset)(height: Int): Key[Option[Unit]] =
    Key.opt(AssetScript, hBytes(asset.id.arr, height), _ => (), _ => Array[Byte]())

  val safeRollbackHeight: Key[Int]   = intKey(SafeRollbackHeight)
  val lastCleanupHeight: Key[Height] = heightKey(LastCleanupHeight)

  def changedDataKeys(height: Int, addressId: AddressId): Key[Seq[String]] =
    Key(ChangedDataKeys, hBytes(addressId.toByteArray, height), readStrings, writeStrings)

  def blockMetaAt(height: Height): Key[Option[PBBlockMeta]] =
    Key.opt(BlockInfoAtHeight, h(height), readBlockMeta, writeBlockMeta)

  def blockInfoBytesAt(height: Height): Key[Option[Array[Byte]]] =
    Key.opt(
      BlockInfoAtHeight,
      h(height),
      identity,
      unsupported("Can not explicitly write block bytes")
    )

  def transactionAt(height: Height, n: TxNum, cfHandle: RDB.TxHandle): Key[Option[(TxMeta, Transaction)]] =
    Key.opt[(TxMeta, Transaction)](
      NthTransactionInfoAtHeight,
      hNum(height, n),
      readTransaction(height),
      writeTransaction,
      Some(cfHandle.handle)
    )

  def transactionStateSnapshotAt(height: Height, n: TxNum, cfHandle: RDB.TxHandle): Key[Option[TransactionStateSnapshot]] =
    Key.opt[TransactionStateSnapshot](
      NthTransactionStateSnapshotAtHeight,
      hNum(height, n),
      TransactionStateSnapshot.parseFrom,
      _.toByteArray,
      Some(cfHandle.handle)
    )

  def addressTransactionSeqNr(addressId: AddressId): Key[Int] =
    bytesSeqNr(AddressTransactionSeqNr, addressId.toByteArray)

  def addressTransactionHN(addressId: AddressId, seqNr: Int): Key[Option[(Height, Seq[(Byte, TxNum, Int)])]] =
    Key.opt(
      AddressTransactionHeightTypeAndNums,
      hBytes(addressId.toByteArray, seqNr),
      readTransactionHNSeqAndType,
      writeTransactionHNSeqAndType
    )

  def addressLeaseSeqNr(addressId: AddressId): Key[Int] =
    bytesSeqNr(AddressLeaseInfoSeqNr, addressId.toByteArray)

  def addressLeaseSeq(addressId: AddressId, seqNr: Int): Key[Option[Seq[ByteStr]]] =
    Key.opt(
      AddressLeaseInfoSeq,
      hBytes(addressId.toByteArray, seqNr),
      readLeaseIdSeq,
      writeLeaseIdSeq
    )

  def transactionMetaById(txId: TransactionId, cfh: RDB.TxMetaHandle): Key[Option[TransactionMeta]] =
    Key.opt(
      TransactionMetaById,
      txId.arr,
      TransactionMeta.parseFrom,
      _.toByteArray,
      Some(cfh.handle)
    )

  def invokeScriptResult(height: Int, txNum: TxNum): Key[Option[InvokeScriptResult]] =
    Key.opt(InvokeScriptResultTag, hNum(height, txNum), InvokeScriptResult.fromBytes, InvokeScriptResult.toBytes)

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

  def nftCount(addressId: AddressId): Key[Int] =
    Key(NftCount, addressId.toByteArray, Option(_).fold(0)(Ints.fromByteArray), Ints.toByteArray)

  def nftAt(addressId: AddressId, index: Int, assetId: IssuedAsset): Key[Option[Unit]] =
    Key.opt(NftPossession, addressId.toByteArray ++ Longs.toByteArray(index) ++ assetId.id.arr, _ => (), _ => Array.emptyByteArray)

  def stateHash(height: Int): Key[Option[StateHash]] =
    Key.opt(StateHash, h(height), readStateHash, writeStateHash)

  def blockStateHash(height: Int): Key[ByteStr] =
    Key(BlockStateHash, h(height), Option(_).fold(TxStateSnapshotHashBuilder.InitStateHash)(ByteStr(_)), _.arr)

  def ethereumTransactionMeta(height: Height, txNum: TxNum): Key[Option[EthereumTransactionMeta]] =
    Key.opt(EthereumTransactionMetaTag, hNum(height, txNum), EthereumTransactionMeta.parseFrom, _.toByteArray)

  def maliciousMinerBanHeights(addressBytes: Array[Byte]): Key[Seq[Int]] =
    historyKey(MaliciousMinerBanHeights, addressBytes)
}
