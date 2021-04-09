package com.wavesplatform.database

import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.protobuf.TransactionMeta
import com.wavesplatform.protobuf.transaction.PBRecipients
import com.wavesplatform.state._
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils._

object Keys {
  import KeyHelpers._
  import KeyTags.{AddressId => AddressIdTag, InvokeScriptResult => InvokeScriptResultTag, LeaseDetails => LeaseDetailsTag, _}

  val version: Key[Int]               = intKey(Version, default = 1)
  val height: Key[Int]                = intKey(Height)
  def score(height: Int): Key[BigInt] = Key(Score, h(height), Option(_).fold(BigInt(0))(BigInt(_)), _.toByteArray)

  def heightOf(blockId: ByteStr): Key[Option[Int]] = Key.opt[Int](HeightOf, blockId.arr, Ints.fromByteArray, Ints.toByteArray)

  def wavesBalanceHistory(addressId: AddressId): Key[Seq[Int]] = historyKey(WavesBalanceHistory, addressId.toByteArray)

  def wavesBalance(addressId: AddressId)(height: Int): Key[Long] =
    Key(WavesBalance, hAddr(height, addressId), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  def assetBalanceHistory(addressId: AddressId, asset: IssuedAsset): Key[Seq[Int]] =
    historyKey(AssetBalanceHistory, addressId.toByteArray ++ asset.id.arr)
  def assetBalance(addressId: AddressId, asset: IssuedAsset)(height: Int): Key[Long] =
    Key(
      AssetBalance,
      hBytes(asset.id.arr ++ addressId.toByteArray, height),
      Option(_).fold(0L)(Longs.fromByteArray),
      Longs.toByteArray
    )

  def assetDetailsHistory(asset: IssuedAsset): Key[Seq[Int]] = historyKey(AssetDetailsHistory, asset.id.arr)
  def assetDetails(asset: IssuedAsset)(height: Int): Key[(AssetInfo, AssetVolumeInfo)] =
    Key(AssetDetails, hBytes(asset.id.arr, height), readAssetDetails, writeAssetDetails)

  def issuedAssets(height: Int): Key[Seq[IssuedAsset]] =
    Key(IssuedAssets, h(height), d => readAssetIds(d).map(IssuedAsset), ias => writeAssetIds(ias.map(_.id)))
  def updatedAssets(height: Int): Key[Seq[IssuedAsset]] =
    Key(UpdatedAssets, h(height), d => readAssetIds(d).map(IssuedAsset), ias => writeAssetIds(ias.map(_.id)))
  def sponsorshipAssets(height: Int): Key[Seq[IssuedAsset]] =
    Key(SponsoredAssets, h(height), d => readAssetIds(d).map(IssuedAsset), ias => writeAssetIds(ias.map(_.id)))


  def leaseBalanceHistory(addressId: AddressId): Key[Seq[Int]] = historyKey(LeaseBalanceHistory, addressId.toByteArray)
  def leaseBalance(addressId: AddressId)(height: Int): Key[LeaseBalance] =
    Key(LeaseBalance, hAddr(height, addressId), readLeaseBalance, writeLeaseBalance)

  def leaseDetailsHistory(leaseId: ByteStr): Key[Seq[Int]] = historyKey(LeaseDetailsHistory, leaseId.arr)
  def leaseDetails(leaseId: ByteStr)(height: Int): Key[Option[Either[Boolean, LeaseDetails]]] =
    Key.opt(LeaseDetailsTag, Ints.toByteArray(height) ++ leaseId.arr, readLeaseDetails, writeLeaseDetails)

  def filledVolumeAndFeeHistory(orderId: ByteStr): Key[Seq[Int]] = historyKey(FilledVolumeAndFeeHistory, orderId.arr)
  def filledVolumeAndFee(orderId: ByteStr)(height: Int): Key[VolumeAndFee] =
    Key(FilledVolumeAndFee, hBytes(orderId.arr, height), readVolumeAndFee, writeVolumeAndFee)

  def changedAddresses(height: Int): Key[Seq[AddressId]] = Key(ChangedAddresses, h(height), readAddressIds, writeAddressIds)

  def changedBalances(height: Int, asset: IssuedAsset): Key[Seq[AddressId]] =
    Key(ChangedAssetBalances, h(height) ++ asset.id.arr, readAddressIds, writeAddressIds)

  def addressIdOfAlias(alias: Alias): Key[Option[AddressId]] = Key.opt(AddressIdOfAlias, alias.bytes, AddressId.fromByteArray, _.toByteArray)

  val lastAddressId: Key[Option[Long]] = Key.opt(LastAddressId, Array.emptyByteArray, Longs.fromByteArray, _.toByteArray)

  def addressId(address: Address): Key[Option[AddressId]] = Key.opt(AddressIdTag, address.bytes, AddressId.fromByteArray, _.toByteArray)
  def idToAddress(addressId: AddressId): Key[Address]     = Key(IdToAddress, addressId.toByteArray, Address.fromBytes(_).explicitGet(), _.bytes)

  def addressScriptHistory(addressId: AddressId): Key[Seq[Int]] = historyKey(AddressScriptHistory, addressId.toByteArray)
  def addressScript(addressId: AddressId)(height: Int): Key[Option[AccountScriptInfo]] =
    Key.opt(AddressScript, hAddr(height, addressId), readAccountScriptInfo, writeAccountScriptInfo)

  val approvedFeatures: Key[Map[Short, Int]]  = Key(ApprovedFeatures, Array.emptyByteArray, readFeatureMap, writeFeatureMap)
  val activatedFeatures: Key[Map[Short, Int]] = Key(ActivatedFeatures, Array.emptyByteArray, readFeatureMap, writeFeatureMap)

  // public key hash is used here so it's possible to populate bloom filter by just scanning all the history keys
  def dataHistory(address: Address, key: String): Key[Seq[Int]] =
    historyKey(DataHistory, PBRecipients.publicKeyHash(address) ++ key.utf8Bytes)
  def data(addressId: AddressId, key: String)(height: Int): Key[Option[DataEntry[_]]] =
    Key.opt(Data, hBytes(addressId.toByteArray ++ key.utf8Bytes, height), readDataEntry(key), writeDataEntry)

  def sponsorshipHistory(asset: IssuedAsset): Key[Seq[Int]] = historyKey(SponsorshipHistory, asset.id.arr)
  def sponsorship(asset: IssuedAsset)(height: Int): Key[SponsorshipValue] =
    Key(Sponsorship, hBytes(asset.id.arr, height), readSponsorship, writeSponsorship)

  def carryFee(height: Int): Key[Long] = Key(CarryFee, h(height), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  def assetScriptHistory(asset: IssuedAsset): Key[Seq[Int]] = historyKey(AssetScriptHistory, asset.id.arr)
  def assetScript(asset: IssuedAsset)(height: Int): Key[Option[AssetScriptInfo]] =
    Key.opt(AssetScript, hBytes(asset.id.arr, height), readAssetScript, writeAssetScript)
  def assetScriptPresent(asset: IssuedAsset)(height: Int): Key[Option[Unit]] =
    Key.opt(AssetScript, hBytes(asset.id.arr, height), _ => (), _ => Array[Byte]())

  val safeRollbackHeight: Key[Int] = intKey(SafeRollbackHeight)

  def changedDataKeys(height: Int, addressId: AddressId): Key[Seq[String]] =
    Key(ChangedDataKeys, hBytes(addressId.toByteArray, height), readStrings, writeStrings)

  def blockMetaAt(height: Height): Key[Option[BlockMeta]] =
    Key.opt(BlockInfoAtHeight, h(height), readBlockMeta, writeBlockMeta)

  def blockInfoBytesAt(height: Height): Key[Option[Array[Byte]]] =
    Key.opt(
      BlockInfoAtHeight,
      h(height),
      identity,
      unsupported("Can not explicitly write block bytes")
    )

  def transactionAt(height: Height, n: TxNum): Key[Option[(Transaction, Boolean)]] =
    Key.opt[(Transaction, Boolean)](
      NthTransactionInfoAtHeight,
      hNum(height, n),
      readTransaction,
      writeTransaction
    )

  def addressTransactionSeqNr(addressId: AddressId): Key[Int] =
    bytesSeqNr(AddressTransactionSeqNr, addressId.toByteArray)

  def addressTransactionHN(addressId: AddressId, seqNr: Int): Key[Option[(Height, Seq[(Byte, TxNum)])]] =
    Key.opt(
      AddressTransactionHeightTypeAndNums,
      hBytes(addressId.toByteArray, seqNr),
      readTransactionHNSeqAndType,
      writeTransactionHNSeqAndType
    )

  def transactionMetaById(txId: TransactionId): Key[Option[TransactionMeta]] =
    Key.opt(
      TransactionMetaById,
      txId.arr,
      TransactionMeta.parseFrom,
      _.toByteArray
    )

  def blockTransactionsFee(height: Int): Key[Long] =
    Key(
      BlockTransactionsFee,
      h(height),
      Longs.fromByteArray,
      Longs.toByteArray
    )

  def invokeScriptResult(height: Int, txNum: TxNum): Key[Option[InvokeScriptResult]] =
    Key.opt(InvokeScriptResultTag, hNum(height, txNum), InvokeScriptResult.fromBytes, InvokeScriptResult.toBytes)

  def blockReward(height: Int): Key[Option[Long]] =
    Key.opt(BlockReward, h(height), Longs.fromByteArray, Longs.toByteArray)

  def wavesAmount(height: Int): Key[BigInt] = Key(WavesAmount, h(height), Option(_).fold(BigInt(0))(BigInt(_)), _.toByteArray)

  def hitSource(height: Int): Key[Option[ByteStr]] = Key.opt(HitSource, h(height), ByteStr(_), _.arr)

  val disabledAliases: Key[Set[Alias]] = Key(
    DisabledAliases,
    Array.emptyByteArray,
    b => readStrings(b).map(s => Alias.create(s).explicitGet()).toSet,
    as => writeStrings(as.map(_.name).toSeq)
  )

  def assetStaticInfo(asset: IssuedAsset): Key[Option[AssetStaticInfo]] =
    Key.opt(AssetStaticInfo, asset.id.arr, readAssetStaticInfo, writeAssetStaticInfo)

  def nftCount(addressId: AddressId): Key[Int] =
    Key(NftCount, addressId.toByteArray, Option(_).fold(0)(Ints.fromByteArray), Ints.toByteArray)

  def nftAt(addressId: AddressId, index: Int, assetId: IssuedAsset): Key[Option[Unit]] =
    Key.opt(NftPossession, addressId.toByteArray ++ Longs.toByteArray(index) ++ assetId.id.arr, _ => (), _ => Array.emptyByteArray)

  def bloomFilterChecksum(filterName: String): Key[Array[Byte]] = Key(KeyTags.BloomFilterChecksum, filterName.utf8Bytes, identity, identity)

  def stateHash(height: Int): Key[Option[StateHash]] =
    Key.opt(StateHash, h(height), readStateHash, writeStateHash)
}
