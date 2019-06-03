package com.wavesplatform.database

import com.google.common.base.Charsets.UTF_8
import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.script.{Script, ScriptReader}
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{Transaction, TransactionParsers}

object Keys {
  import KeyHelpers._

  val version: Key[Int]               = intKey("version", 0, default = 1)
  val height: Key[Int]                = intKey("height", 1)
  def score(height: Int): Key[BigInt] = Key("score", h(2, height), Option(_).fold(BigInt(0))(BigInt(_)), _.toByteArray)

  def heightOf(blockId: ByteStr): Key[Option[Int]] = Key.opt[Int]("height-of", hash(4, blockId), Ints.fromByteArray, Ints.toByteArray)

  def wavesBalanceHistory(addressId: AddressId): Key[Seq[Int]] = historyKey("waves-balance-history", 5, writeAddressId(addressId))
  def wavesBalance(addressId: AddressId)(height: Int): Key[Long] =
    Key("waves-balance", hAddr(6, height, addressId), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  def assetList(addressId: AddressId): Key[List[IssuedAsset]] =
    Key("asset-list", addr(7, addressId), readTxIds(_).map(IssuedAsset), assets => writeTxIds(assets.map(_.id)))
  def assetBalanceHistory(addressId: AddressId, asset: IssuedAsset): Key[Seq[Int]] =
    historyKey("asset-balance-history", 8, writeAddressId(addressId) ++ asset.id.arr)

  def assetBalance(addressId: AddressId, asset: IssuedAsset)(height: Int): Key[Long] =
    Key("asset-balance", hBytes(9, height, writeAddressId(addressId) ++ asset.id.arr), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  def assetInfoHistory(asset: IssuedAsset): Key[Seq[Int]] = historyKey("asset-info-history", 10, asset.id.arr)
  def assetInfo(asset: IssuedAsset)(height: Int): Key[AssetInfo] =
    Key("asset-info", hBytes(11, height, asset.id.arr), readAssetInfo, writeAssetInfo)

  def leaseBalanceHistory(addressId: AddressId): Key[Seq[Int]] = historyKey("lease-balance-history", 12, writeAddressId(addressId))
  def leaseBalance(addressId: AddressId)(height: Int): Key[LeaseBalance] =
    Key("lease-balance", hAddr(13, height, addressId), readLeaseBalance, writeLeaseBalance)
  def leaseStatusHistory(leaseId: ByteStr): Key[Seq[Int]] = historyKey("lease-status-history", 14, leaseId.arr)
  def leaseStatus(leaseId: ByteStr)(height: Int): Key[Boolean] =
    Key("lease-status", hBytes(15, height, leaseId.arr), _(0) == 1, active => Array[Byte](if (active) 1 else 0))

  def filledVolumeAndFeeHistory(orderId: ByteStr): Key[Seq[Int]] = historyKey("filled-volume-and-fee-history", 16, orderId.arr)
  def filledVolumeAndFee(orderId: ByteStr)(height: Int): Key[VolumeAndFee] =
    Key("filled-volume-and-fee", hBytes(17, height, orderId.arr), readVolumeAndFee, writeVolumeAndFee)

  // 19, 20 were never used

  def changedAddresses(height: Int): Key[Seq[AddressId]] = Key("changed-addresses", h(21, height), readAddressIdSeq, writeAddressIdSeq)

  def addressIdOfAlias(alias: Alias): Key[Option[AddressId]] = Key.opt("address-id-of-alias", bytes(23, alias.bytes.arr), readAddressId, writeAddressId)

  val lastAddressId: Key[Option[AddressId]] = Key.opt("last-address-id", Array[Byte](0, 24), readAddressId, writeAddressId)

  def addressId(address: Address): Key[Option[AddressId]] = Key.opt("address-id", bytes(25, address.bytes.arr), readAddressId, writeAddressId)
  def idToAddress(id: AddressId): Key[Address]            = Key("id-to-address", bytes(26, writeAddressId(id)), Address.fromBytes(_).explicitGet(), _.bytes.arr)

  def addressScriptHistory(addressId: AddressId): Key[Seq[Int]] = historyKey("address-script-history", 27, writeAddressId(addressId))
  def addressScript(addressId: AddressId)(height: Int): Key[Option[Script]] =
    Key.opt("address-script", hAddr(28, height, addressId), ScriptReader.fromBytes(_).explicitGet(), _.bytes().arr)

  val approvedFeatures: Key[Map[Short, Int]]  = Key("approved-features", Array[Byte](0, 29), readFeatureMap, writeFeatureMap)
  val activatedFeatures: Key[Map[Short, Int]] = Key("activated-features", Array[Byte](0, 30), readFeatureMap, writeFeatureMap)

  def dataKeyChunkCount(addressId: AddressId): Key[Int] =
    Key("data-key-chunk-count", addr(31, addressId), Option(_).fold(0)(Ints.fromByteArray), Ints.toByteArray)
  def dataKeyChunk(addressId: AddressId, chunkNo: Int): Key[Seq[String]] =
    Key("data-key-chunk", addr(32, addressId) ++ Ints.toByteArray(chunkNo), readStrings, writeStrings)

  def dataHistory(addressId: AddressId, key: String): Key[Seq[Int]] = historyKey("data-history", 33, writeAddressId(addressId) ++ key.getBytes(UTF_8))
  def data(addressId: AddressId, key: String)(height: Int): Key[Option[DataEntry[_]]] =
    Key.opt("data", hBytes(34, height, writeAddressId(addressId) ++ key.getBytes(UTF_8)), DataEntry.parseValue(key, _, 0)._1, _.valueBytes)

  def sponsorshipHistory(asset: IssuedAsset): Key[Seq[Int]] = historyKey("sponsorship-history", 35, asset.id.arr)
  def sponsorship(asset: IssuedAsset)(height: Int): Key[SponsorshipValue] =
    Key("sponsorship", hBytes(36, height, asset.id.arr), readSponsorship, writeSponsorship)

  val addressesForWavesSeqNr: Key[Int]                   = intKey("addresses-for-waves-seq-nr", 37)
  def addressesForWaves(seqNr: Int): Key[Seq[AddressId]] = Key("addresses-for-waves", h(38, seqNr), readAddressIdSeq, writeAddressIdSeq)

  def addressesForAssetSeqNr(asset: IssuedAsset): Key[Int] = bytesSeqNr("addresses-for-asset-seq-nr", 39, asset.id.arr)
  def addressesForAsset(asset: IssuedAsset, seqNr: Int): Key[Seq[AddressId]] =
    Key("addresses-for-asset", hBytes(40, seqNr, asset.id.arr), readAddressIdSeq, writeAddressIdSeq)

  val AliasIsDisabledPrefix: Short = 43
  def aliasIsDisabled(alias: Alias): Key[Boolean] =
    Key("alias-is-disabled", bytes(AliasIsDisabledPrefix, alias.bytes.arr), Option(_).exists(_(0) == 1), if (_) Array[Byte](1) else Array[Byte](0))

  /* 44: carryFeeHistory, obsolete */
  def carryFee(height: Int): Key[Long] = Key("carry-fee", h(45, height), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  def assetScriptHistory(asset: IssuedAsset): Key[Seq[Int]] = historyKey("asset-script-history", 46, asset.id.arr)
  def assetScript(asset: IssuedAsset)(height: Int): Key[Option[Script]] =
    Key.opt("asset-script", hBytes(47, height, asset.id.arr), ScriptReader.fromBytes(_).explicitGet(), _.bytes().arr)
  def assetScriptPresent(asset: IssuedAsset)(height: Int): Key[Option[Unit]] =
    Key.opt("asset-script", hBytes(47, height, asset.id.arr), _ => (), _ => Array[Byte]())

  val safeRollbackHeight: Key[Int] = intKey("safe-rollback-height", 48)

  def changedDataKeys(height: Int, addressId: AddressId): Key[Seq[String]] =
    Key("changed-data-keys", hAddr(49, height, addressId), readStrings, writeStrings)

  val BlockHeaderPrefix: Short = 50

  def blockHeaderAndSizeAt(height: Height): Key[Option[(BlockHeader, Int)]] =
    Key.opt("block-header-at-height", h(BlockHeaderPrefix, height), readBlockHeaderAndSize, writeBlockHeaderAndSize)

  def blockHeaderBytesAt(height: Height): Key[Option[Array[Byte]]] =
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
      data => TransactionParsers.parseBytes(data).get,
      _.bytes()
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
    bytesSeqNr("address-transaction-seq-nr", AddressTransactionSeqNrPrefix, writeAddressId(addressId))

  val AddressTransactionHNPrefix: Short = 53
  def addressTransactionHN(addressId: AddressId, seqNr: Int): Key[Option[(Height, Seq[(Byte, TxNum)])]] =
    Key.opt(
      "address-transaction-height-type-and-nums",
      hBytes(AddressTransactionHNPrefix, seqNr, writeAddressId(addressId)),
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
}
