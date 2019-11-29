package com.wavesplatform.database

import com.google.common.base.Charsets.UTF_8
import com.google.common.primitives.{Ints, Longs, Shorts}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.script.Script
import com.wavesplatform.protobuf.transaction.{PBSignedTransaction, PBTransactions}
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.Transaction

object Keys {
  import KeyHelpers._

  val version: Key[Int]               = intKey("version", 0, default = 1)
  val height: Key[Int]                = intKey("height", 1)
  def score(height: Int): Key[BigInt] = Key("score", h(2, height), Option(_).fold(BigInt(0))(BigInt(_)), _.toByteArray)

  def heightOf(blockId: ByteStr): Key[Option[Int]] = Key.opt[Int]("height-of", hash(4, blockId), Ints.fromByteArray, Ints.toByteArray)

  def wavesBalanceHistory(addressId: BigInt): Key[Seq[Int]] = historyKey("waves-balance-history", 5, addressId.toByteArray)

  val WavesBalancePrefix: Short = 6
  def wavesBalance(addressId: BigInt)(height: Int): Key[Long] =
    Key("waves-balance", hAddr(WavesBalancePrefix, height, addressId), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  val AssetBalanceHistoryPrefix = 8.toShort
  def assetBalanceHistory(addressId: BigInt, asset: IssuedAsset): Key[Seq[Int]] =
    historyKey("asset-balance-history", AssetBalanceHistoryPrefix, addressId.toByteArray ++ asset.id.arr)
  val AssetBalancePrefix: Short = 9
  def assetBalance(addressId: BigInt, asset: IssuedAsset)(height: Int): Key[Long] =
    Key(
      "asset-balance",
      hBytes(AssetBalancePrefix,  asset.id.arr ++ addressId.toByteArray, height),
      Option(_).fold(0L)(Longs.fromByteArray),
      Longs.toByteArray
    )

  def assetInfoHistory(asset: IssuedAsset): Key[Seq[Int]] = historyKey("asset-info-history", 10, asset.id.arr)
  def assetInfo(asset: IssuedAsset)(height: Int): Key[AssetInfo] =
    Key("asset-info", hBytes(11, asset.id.arr, height), readAssetInfo, writeAssetInfo)

  def leaseBalanceHistory(addressId: BigInt): Key[Seq[Int]] = historyKey("lease-balance-history", 12, addressId.toByteArray)
  def leaseBalance(addressId: BigInt)(height: Int): Key[LeaseBalance] =
    Key("lease-balance", hAddr(13, height, addressId), readLeaseBalance, writeLeaseBalance)
  def leaseStatusHistory(leaseId: ByteStr): Key[Seq[Int]] = historyKey("lease-status-history", 14, leaseId.arr)
  val LeaseStatusPrefix: Short                            = 15
  def leaseStatus(leaseId: ByteStr)(height: Int): Key[Boolean] =
    Key("lease-status", hBytes(LeaseStatusPrefix, leaseId.arr, height), _(0) == 1, active => Array[Byte](if (active) 1 else 0))

  def filledVolumeAndFeeHistory(orderId: ByteStr): Key[Seq[Int]] = historyKey("filled-volume-and-fee-history", 16, orderId.arr)
  def filledVolumeAndFee(orderId: ByteStr)(height: Int): Key[VolumeAndFee] =
    Key("filled-volume-and-fee", hBytes(17, orderId.arr, height), readVolumeAndFee, writeVolumeAndFee)

  // 19, 20 were never used

  def changedAddresses(height: Int): Key[Seq[BigInt]] = Key("changed-addresses", h(21, height), readBigIntSeq, writeBigIntSeq)

  def addressIdOfAlias(alias: Alias): Key[Option[BigInt]] = Key.opt("address-id-of-alias", bytes(23, alias.bytes.arr), BigInt(_), _.toByteArray)

  val lastAddressId: Key[Option[BigInt]] = Key.opt("last-address-id", Array[Byte](0, 24), BigInt(_), _.toByteArray)

  def addressId(address: Address): Key[Option[BigInt]] = Key.opt("address-id", bytes(25, address.bytes.arr), BigInt(_), _.toByteArray)
  def idToAddress(id: BigInt): Key[Address]            = Key("id-to-address", bytes(26, id.toByteArray), Address.fromBytes(_).explicitGet(), _.bytes.arr)

  def addressScriptHistory(addressId: BigInt): Key[Seq[Int]] = historyKey("address-script-history", 27, addressId.toByteArray)
  def addressScript(addressId: BigInt)(height: Int): Key[Option[AccountScriptInfo]] =
    Key.opt("address-script", hAddr(28, height, addressId), readScript, writeScript)

  val approvedFeatures: Key[Map[Short, Int]]  = Key("approved-features", Array[Byte](0, 29), readFeatureMap, writeFeatureMap)
  val activatedFeatures: Key[Map[Short, Int]] = Key("activated-features", Array[Byte](0, 30), readFeatureMap, writeFeatureMap)

  def dataKeyChunkCount(addressId: BigInt): Key[Int] =
    Key("data-key-chunk-count", addr(31, addressId), Option(_).fold(0)(Ints.fromByteArray), Ints.toByteArray)
  def dataKeyChunk(addressId: BigInt, chunkNo: Int): Key[Seq[String]] =
    Key("data-key-chunk", addr(32, addressId) ++ Ints.toByteArray(chunkNo), readStrings, writeStrings)

  val DataHistoryPrefix: Short                                   = 33
  def dataHistory(addressId: BigInt, key: String): Key[Seq[Int]] = historyKey("data-history", 33, addressId.toByteArray ++ key.getBytes(UTF_8))
  def data(addressId: BigInt, key: String)(height: Int): Key[Option[DataEntry[_]]] =
    Key.opt("data", hBytes(34, addressId.toByteArray ++ key.getBytes(UTF_8), height), DataEntry.parseValue(key, _, 0)._1, _.valueBytes)

  def sponsorshipHistory(asset: IssuedAsset): Key[Seq[Int]] = historyKey("sponsorship-history", 35, asset.id.arr)
  def sponsorship(asset: IssuedAsset)(height: Int): Key[SponsorshipValue] =
    Key("sponsorship", hBytes(36, asset.id.arr, height), readSponsorship, writeSponsorship)

  /* 37: addressesForWavesSeqNr, obsolete
   * 38: addressesForWaves, obsolete
   * 39: addressesForAssetSeqNr, obsolete
   * 40: addressesForAsset, obsolete
   * 41, 42 were never used
   * 43: aliasIsDisabled, obsolete
   * 44: carryFeeHistory, obsolete */

  def carryFee(height: Int): Key[Long] = Key("carry-fee", h(45, height), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  def assetScriptHistory(asset: IssuedAsset): Key[Seq[Int]] = historyKey("asset-script-history", 46, asset.id.arr)
  def assetScript(asset: IssuedAsset)(height: Int): Key[Option[(Script, Long)]] =
    Key.opt("asset-script", hBytes(47, asset.id.arr, height), readAssetScript, writeAssetScript)
  def assetScriptPresent(asset: IssuedAsset)(height: Int): Key[Option[Unit]] =
    Key.opt("asset-script", hBytes(47, asset.id.arr, height), _ => (), _ => Array[Byte]())

  val safeRollbackHeight: Key[Int] = intKey("safe-rollback-height", 48)

  def changedDataKeys(height: Int, addressId: BigInt): Key[Seq[String]] =
    Key("changed-data-keys", hAddr(49, height, addressId), readStrings, writeStrings)

  val BlockHeaderPrefix: Short = 50

  def blockMetaAt(height: Height): Key[Option[BlockMeta]] =
    Key.opt("block-header-at-height", h(BlockHeaderPrefix, height), readBlockMeta(height), writeBlockMeta)

  def blockHeaderBytesAt(height: Height): Key[Option[Array[Byte]]] = // TODO: Store protobuf block header
    Key.opt(
      "block-header-bytes-at-height",
      h(BlockHeaderPrefix, height),
      _.drop(4),
      _ => throw new Exception("Key \"block-header-bytes-at-height\" - is read only!")
    )

  val TransactionInfoPrefix: Short = 51
  def transactionAt(height: Height, n: TxNum): Key[Option[Transaction]] =
    Key.opt[Transaction](
      "nth-transaction-info-at-height",
      hNum(TransactionInfoPrefix, height, n),
      data => PBTransactions.vanillaUnsafe(PBSignedTransaction.parseFrom(data)),
      tx => PBUtils.encodeDeterministic(PBTransactions.protobuf(tx))
    )

  def transactionBytesAt(height: Height, n: TxNum): Key[Option[Array[Byte]]] =
    Key.opt(
      "nth-transaction-info-bytes-at-height",
      hNum(TransactionInfoPrefix, height, n),
      identity,
      identity
    )

  val AddressTransactionSeqNrPrefix: Short = 52
  def addressTransactionSeqNr(addressId: AddressId): Key[Int] =
    bytesSeqNr("address-transaction-seq-nr", AddressTransactionSeqNrPrefix, addressId.toByteArray)

  val AddressTransactionHNPrefix: Short = 53
  def addressTransactionHN(addressId: AddressId, seqNr: Int): Key[Option[(Height, Seq[(Byte, TxNum)])]] =
    Key.opt(
      "address-transaction-height-type-and-nums",
      hBytes(AddressTransactionHNPrefix, addressId.toByteArray, seqNr),
      readTransactionHNSeqAndType,
      writeTransactionHNSeqAndType
    )

  val TransactionHeightNumByIdPrefix: Short = 54
  def transactionHNById(txId: TransactionId): Key[Option[(Height, TxNum)]] =
    Key.opt(
      "transaction-height-and-nums-by-id",
      bytes(TransactionHeightNumByIdPrefix, txId.arr),
      readTransactionHN,
      writeTransactionHN
    )

  val BlockTransactionsFeePrefix: Short = 55
  def blockTransactionsFee(height: Int): Key[Long] =
    Key(
      "block-transactions-fee",
      h(BlockTransactionsFeePrefix, height),
      Longs.fromByteArray,
      Longs.toByteArray
    )

  val InvokeScriptResultPrefix: Short = 56
  def invokeScriptResult(height: Int, txNum: TxNum): Key[InvokeScriptResult] =
    Key("invoke-script-result", hNum(InvokeScriptResultPrefix, height, txNum), InvokeScriptResult.fromBytes, InvokeScriptResult.toBytes)

  val BlockRewardPrefix: Short = 57
  def blockReward(height: Int): Key[Option[Long]] =
    Key.opt("block-reward", h(BlockRewardPrefix, height), Longs.fromByteArray, Longs.toByteArray)

  val wavesAmountPrefix: Short              = 58
  def wavesAmount(height: Int): Key[BigInt] = Key("waves-amount", h(wavesAmountPrefix, height), Option(_).fold(BigInt(0))(BigInt(_)), _.toByteArray)

  val HitSourcePrefix: Short                       = 59
  def hitSource(height: Int): Key[Option[ByteStr]] = Key.opt("hit-source", h(HitSourcePrefix, height), ByteStr(_), _.arr)

  val disabledAliases: Key[Set[Alias]] = Key(
    "disabled-aliases",
    Shorts.toByteArray(60.toShort),
    b => readStrings(b).map(s => Alias.create(s).explicitGet()).toSet,
    as => writeStrings(as.map(_.name).toSeq)
  )
}
