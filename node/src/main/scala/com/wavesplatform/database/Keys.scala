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

  def parseAddressBytesHeight(bs: Array[Byte]): (Short, AddressId, Array[Byte], Height) = {
    val prefix = Shorts.fromByteArray(bs.take(2))
    val addressId = AddressId.fromBytes(bs.slice(2, 6))
    val height = Height(Ints.fromByteArray(bs.takeRight(4)))
    val aux = bs.drop(6).dropRight(4)
    (prefix, addressId, aux, height)
  }

  def wavesBalanceLastHeight(addressId: Long): Key[Int] =
    Key("waves-balance-last-height", addr(5, addressId), Option(_).fold(0)(Ints.fromByteArray), Ints.toByteArray)

  val WavesBalancePrefix: Short = 6
  def wavesBalance(addressId: Long)(height: Int): Key[Long] =
    Key("waves-balance", hAddr(WavesBalancePrefix, addressId, height), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  def assetList(addressId: AddressId): Key[List[IssuedAsset]] =
    Key("asset-list", addr(7, addressId), readTxIds(_).map(IssuedAsset), assets => writeTxIds(assets.map(_.id)))

  def assetBalanceLastHeight(addressId: Long, asset: IssuedAsset): Key[Int] =
    Key("asset-balance-last-height",
      bytes(8, Bytes.concat(AddressId.toBytes(addressId), asset.id)),
      Option(_).fold(0)(Ints.fromByteArray),
      Ints.toByteArray)

  val AssetBalancePrefix: Short = 9
  def assetBalance(addressId: AddressId, issueTxHeight: Height, issueTxNum: TxNum)(height: Int): Key[Long] = {
    val keyBytes =
      hBytes(
        AssetBalancePrefix,
        Bytes.concat(
          writeAddressId(addressId),
          Ints.toByteArray(issueTxHeight),
          Shorts.toByteArray(issueTxNum)
        ),
	height
      )

    val balanceDecoder = (arr: Array[Byte]) => Option(arr).fold(0L)(Longs.fromByteArray)
    val balanceEncoder = (b: Long) => Longs.toByteArray(b)

    Key("asset-balance", keyBytes, balanceDecoder, balanceEncoder)
  }


  val AssetInfoPrefix: Short = 11
  def assetInfo(asset: IssuedAsset)(height: Int): Key[AssetInfo] =
    Key("asset-info", hBytes(AssetInfoPrefix, asset.id.arr, height), readAssetInfo, writeAssetInfo)

  def leaseBalanceLastHeight(addressId: Long): Key[Int] =
  def leaseBalanceHistory(addressId: BigInt): Key[Seq[Int]] = historyKey("lease-balance-history", 12, addressId.toByteArray)

  val LeaseBalancePrefix: Short = 13
  def leaseBalance(addressId: BigInt)(height: Int): Key[LeaseBalance] =
    Key("lease-balance", hAddr(LeaseBalancePrefix, addressId, height), readLeaseBalance, writeLeaseBalance)

  val LeaseStatusPrefix: Short = 15
  def leaseStatus(leaseId: ByteStr)(height: Int): Key[Boolean] =
    Key("lease-status", hBytes(LeaseStatusPrefix, leaseId.arr, height), _(0) == 1, active => Array[Byte](if (active) 1 else 0))

  val FilledVolumeAndFeePrefix: Short = 17
  def filledVolumeAndFee(orderId: ByteStr)(height: Int): Key[VolumeAndFee] =
    Key("filled-volume-and-fee", hBytes(FilledVolumeAndFeePrefix, orderId.arr, height), readVolumeAndFee, writeVolumeAndFee)

  // 3, 10, 14, 16, 18, 22, 27, 33, 35, 37, 38, 41, 42, 44, 46 not used
  // 19, 20 were never used

  def changedAddresses(height: Int): Key[Seq[AddressId]] = Key("changed-addresses", h(21, height), readAddressIdSeq, writeAddressIdSeq)

  def addressIdOfAlias(alias: Alias): Key[Option[AddressId]] =
    Key.opt("address-id-of-alias", bytes(23, alias.bytes.arr), readAddressId, writeAddressId)

  val lastAddressId: Key[Option[AddressId]] = Key.opt("last-address-id", Array[Byte](0, 24), readAddressId, writeAddressId)

  def addressId(address: Address): Key[Option[AddressId]] = Key.opt("address-id", bytes(25, address.bytes.arr), readAddressId, writeAddressId)
  def idToAddress(id: AddressId): Key[Address]            = Key("id-to-address", bytes(26, writeAddressId(id)), Address.fromBytes(_).explicitGet(), _.bytes.arr)

  val AddressScriptPrefix: Short = 28
  def addressScript(addressId: AddressId)(height: Int): Key[Option[Script]] =
    Key.opt("address-script", hAddr(AddressScriptPrefix, addressId, height), ScriptReader.fromBytes(_).explicitGet(), _.bytes().arr)

  val approvedFeatures: Key[Map[Short, Int]]  = Key("approved-features", Array[Byte](0, 29), readFeatureMap, writeFeatureMap)
  val activatedFeatures: Key[Map[Short, Int]] = Key("activated-features", Array[Byte](0, 30), readFeatureMap, writeFeatureMap)

  def dataKeyChunkCount(addressId: AddressId): Key[Int] =
    Key("data-key-chunk-count", addr(31, addressId), Option(_).fold(0)(Ints.fromByteArray), Ints.toByteArray)
  def dataKeyChunk(addressId: AddressId, chunkNo: Int): Key[Seq[String]] =
    Key("data-key-chunk", addr(32, addressId) ++ Ints.toByteArray(chunkNo), readStrings, writeStrings)

  val DataPrefix: Short = 34
  def data(addressId: AddressId, key: String)(height: Int): Key[Option[DataEntry[_]]] =
    Key.opt("data",
    Key.opt("data", hBytes(34, height, writeAddressId(addressId) ++ key.getBytes(UTF_8)), DataEntry.parseValue(key, _, 0)._1, _.valueBytes)
      DataEntry.parseValue(key, _, 0)._1,
      _.valueBytes)

  val SponsorshipPrefix: Short = 36
  def sponsorship(asset: IssuedAsset)(height: Int): Key[SponsorshipValue] =
    Key("sponsorship", hBytes(SponsorshipPrefix, asset.id.arr, height), readSponsorship, writeSponsorship)

  def addressesForWaves(seqNr: Int): Key[Seq[AddressId]] = Key("addresses-for-waves", h(38, seqNr), readAddressIdSeq, writeAddressIdSeq)
  def addressesForAssetSeqNr(asset: IssuedAsset): Key[Int] = bytesSeqNr("addresses-for-asset-seq-nr", 39, asset.id.arr)
  def addressesForAsset(asset: IssuedAsset, seqNr: Int): Key[Seq[AddressId]] =
    Key("addresses-for-asset", hBytes(40, seqNr, asset.id.arr), readAddressIdSeq, writeAddressIdSeq)

  val AliasIsDisabledPrefix: Short = 43
  def aliasIsDisabled(alias: Alias): Key[Boolean] =
    Key("alias-is-disabled", bytes(AliasIsDisabledPrefix, alias.bytes.arr), Option(_).exists(_(0) == 1), if (_) Array[Byte](1) else Array[Byte](0))

  /* 44: carryFeeHistory, obsolete */
  def carryFee(height: Int): Key[Long] = Key("carry-fee", h(45, height), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  val AssetScriptPrefix: Short = 47
  def assetScript(asset: IssuedAsset)(height: Int): Key[Option[Script]] =
    Key.opt("asset-script", hBytes(AssetScriptPrefix, asset.id.arr, height), ScriptReader.fromBytes(_).explicitGet(), _.bytes().arr)

  val safeRollbackHeight: Key[Int] = intKey("safe-rollback-height", 48)

  def changedDataKeys(height: Int, addressId: AddressId): Key[Seq[String]] =
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
    bytesSeqNr("address-transaction-seq-nr", AddressTransactionSeqNrPrefix, writeAddressId(addressId))

  val AddressTransactionHNPrefix: Short = 53
  def addressTransactionHN(addressId: AddressId, seqNr: Int): Key[Option[(Height, Seq[(Byte, TxNum)])]] =
    Key.opt(
      "address-transaction-height-type-and-nums",
      hBytes(AddressTransactionHNPrefix, writeAddressId(addressId), seqNr),
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
