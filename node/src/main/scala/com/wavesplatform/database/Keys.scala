package com.wavesplatform.database

import com.google.common.base.Charsets.UTF_8
import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
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

  def parseBytesHeight(bs: Array[Byte]): (Short, Array[Byte], Height) = {
    val prefix = Shorts.fromByteArray(bs.take(2))
    val height = Height(Ints.fromByteArray(bs.takeRight(4)))
    val aux = bs.drop(2).dropRight(4)
    (prefix, aux, height)
  }

  def parseAddressBytesHeight(bs: Array[Byte]): (Short, AddressId, Array[Byte], Height) = {
    val prefix = Shorts.fromByteArray(bs.take(2))
    val addressId = AddressId.fromBytes(bs.slice(2, 6))
    val height = Height(Ints.fromByteArray(bs.takeRight(4)))
    val aux = bs.drop(6).dropRight(4)
    (prefix, addressId, aux, height)
  }

  val WavesBalancePrefix: Short = 6
  def wavesBalance(addressId: Long)(height: Int): Key[Long] =
    Key("waves-balance", hAddr(WavesBalancePrefix, addressId, height), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  def assetList(addressId: Long): Key[List[IssuedAsset]] =
    Key("asset-list", addr(7, addressId), readTxIds(_).map(IssuedAsset), assets => writeTxIds(assets.map(_.id)))

  val AssetBalancePrefix: Short = 9
  def assetBalancePrefix(addressId: Long, asset: IssuedAsset): Array[Byte] = bytes(AssetBalancePrefix, Bytes.concat(Ints.toByteArray(addressId.toInt)))
  def assetBalance(addressId: Long, asset: IssuedAsset)(height: Int): Key[Long] =
    Key("asset-balance", hBytes(AssetBalancePrefix, Bytes.concat(Ints.toByteArray(addressId.toInt), asset.id.arr), height), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  val AssetInfoPrefix: Short = 11
  def assetInfo(asset: IssuedAsset)(height: Int): Key[AssetInfo] =
    Key("asset-info", hBytes(AssetInfoPrefix, asset.id.arr, height), readAssetInfo, writeAssetInfo)

  val LeaseBalancePrefix: Short = 13
  def leaseBalance(addressId: Long)(height: Int): Key[LeaseBalance] =
    Key("lease-balance", hAddr(LeaseBalancePrefix, addressId, height), readLeaseBalance, writeLeaseBalance)

  val LeaseStatusPrefix: Short = 15
  def leaseStatus(leaseId: ByteStr)(height: Int): Key[Boolean] =
    Key("lease-status", hBytes(LeaseStatusPrefix, leaseId.arr, height), _(0) == 1, active => Array[Byte](if (active) 1 else 0))

  val FilledVolumeAndFeePrefix: Short = 17
  def filledVolumeAndFee(orderId: ByteStr)(height: Int): Key[VolumeAndFee] =
    Key("filled-volume-and-fee", hBytes(FilledVolumeAndFeePrefix, orderId.arr, height), readVolumeAndFee, writeVolumeAndFee)

  // 19, 20 were never used

  def changedAddresses(height: Int): Key[Seq[Long]] = Key("changed-addresses", h(21, height), AddressId.readSeq, AddressId.writeSeq)

  def addressIdOfAlias(alias: Alias): Key[Option[Long]] = Key.opt("address-id-of-alias", bytes(23, alias.bytes.arr), AddressId.fromBytes, AddressId.toBytes)

  val lastAddressId: Key[Option[Long]] = Key.opt("last-address-id", Array[Byte](0, 24), AddressId.fromBytes, AddressId.toBytes)

  def addressId(address: Address): Key[Option[Long]] = Key.opt("address-id", bytes(25, address.bytes.arr), b => AddressId.fromBytes(b), id => AddressId.toBytes(id))
  def idToAddress(id: Long): Key[Address]            = Key("id-to-address", bytes(26, AddressId.toBytes(id)), Address.fromBytes(_).explicitGet(), _.bytes.arr)

  val AddressScriptPrefix: Short = 28
  def addressScript(addressId: Long)(height: Int): Key[Option[Script]] =
    Key.opt("address-script", hAddr(AddressScriptPrefix, addressId, height), ScriptReader.fromBytes(_).explicitGet(), _.bytes().arr)

  val approvedFeatures: Key[Map[Short, Int]]  = Key("approved-features", Array[Byte](0, 29), readFeatureMap, writeFeatureMap)
  val activatedFeatures: Key[Map[Short, Int]] = Key("activated-features", Array[Byte](0, 30), readFeatureMap, writeFeatureMap)

  def dataKeyChunkCount(addressId: Long): Key[Int] =
    Key("data-key-chunk-count", addr(31, addressId), Option(_).fold(0)(Ints.fromByteArray), Ints.toByteArray)
  def dataKeyChunk(addressId: Long, chunkNo: Int): Key[Seq[String]] =
    Key("data-key-chunk", addr(32, addressId) ++ Ints.toByteArray(chunkNo), readStrings, writeStrings)

  def dataHistory(addressId: Long, key: String): Key[Seq[Int]] = historyKey("data-history", 33, Bytes.concat(AddressId.toBytes(addressId), key.getBytes(UTF_8)))
  def data(addressId: Long, key: String)(height: Int): Key[Option[DataEntry[_]]] =
    Key.opt("data", hBytes(34, Bytes.concat(AddressId.toBytes(addressId), key.getBytes(UTF_8)), height), DataEntry.parseValue(key, _, 0)._1, _.valueBytes)

  val SponsorshipPrefix: Short = 36
  def sponsorship(asset: IssuedAsset)(height: Int): Key[SponsorshipValue] =
    Key("sponsorship", hBytes(SponsorshipPrefix, asset.id.arr, height), readSponsorship, writeSponsorship)

  val addressesForWavesSeqNr: Key[Int]                = intKey("addresses-for-waves-seq-nr", 37)
  def addressesForWaves(seqNr: Int): Key[Seq[Long]] = Key("addresses-for-waves", h(38, seqNr), AddressId.readSeq, AddressId.writeSeq)

  def addressesForAssetSeqNr(asset: IssuedAsset): Key[Int] = bytesSeqNr("addresses-for-asset-seq-nr", 39, asset.id.arr)
  def addressesForAsset(asset: IssuedAsset, seqNr: Int): Key[Seq[Long]] =
    Key("addresses-for-asset", hBytes(40, asset.id.arr, seqNr), AddressId.readSeq, AddressId.writeSeq)

  val AliasIsDisabledPrefix: Short = 43
  def aliasIsDisabled(alias: Alias): Key[Boolean] =
    Key("alias-is-disabled", bytes(AliasIsDisabledPrefix, alias.bytes.arr), Option(_).exists(_(0) == 1), if (_) Array[Byte](1) else Array[Byte](0))

  /* 44: carryFeeHistory, obsolete */
  def carryFee(height: Int): Key[Long] = Key("carry-fee", h(45, height), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  val AssetScriptPrefix: Short = 47
  def assetScript(asset: IssuedAsset)(height: Int): Key[Option[Script]] =
    Key.opt("asset-script", hBytes(AssetScriptPrefix, asset.id.arr, height), ScriptReader.fromBytes(_).explicitGet(), _.bytes().arr)
  def assetScriptPresent(asset: IssuedAsset)(height: Int): Key[Option[Unit]] =
    Key.opt("asset-script", hBytes(AssetScriptPrefix, asset.id.arr, height), (_ => ()), (_ => Array[Byte]()))

  val safeRollbackHeight: Key[Int] = intKey("safe-rollback-height", 48)

  def changedDataKeys(height: Int, addressId: Long): Key[Seq[String]] =
    Key("changed-data-keys", hAddr(49, addressId, height), readStrings, writeStrings)

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
    bytesSeqNr("address-transaction-seq-nr", AddressTransactionSeqNrPrefix, AddressId.toBytes(addressId))

  val AddressTransactionHNPrefix: Short = 53
  def addressTransactionHN(addressId: AddressId, seqNr: Int): Key[Option[(Height, Seq[(Byte, TxNum)])]] =
    Key.opt(
      "address-transaction-height-type-and-nums",
      hBytes(AddressTransactionHNPrefix, AddressId.toBytes(addressId), seqNr),
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
