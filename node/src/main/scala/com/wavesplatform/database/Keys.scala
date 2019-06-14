package com.wavesplatform.database

import com.google.common.primitives.{Ints, Longs, Shorts}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.script.{Script, ScriptReader}
import com.wavesplatform.state._
import com.wavesplatform.transaction.{Transaction, TransactionParsers}

object Keys {

  import KeyDsl.Implicits._
  import KeyDsl.KeyRW
  import KeyHelpers._
  import shapeless._

  val version: Key[Int]               = intKey("version", 0, default = 1)
  val height: Key[Int]                = intKey("height", 1)

  def score(height: Int): Key[BigInt] = Key("score", (2.toShort, height), Option(_).fold(BigInt(0))(BigInt(_)), _.toByteArray)

  def heightOf(blockId: ByteStr): Key[Option[Int]] = Key.opt("height-of", (4.toShort, blockId.arr), Ints.fromByteArray, Ints.toByteArray)

  def parseAddressBytesHeight(bs: Array[Byte]): (Short, AddressId, Array[Byte], Height) = {
    val prefix = Shorts.fromByteArray(bs.take(2))
    val addressId = AddressId.fromBytes(bs.slice(2, 6))
    val height = Height(Ints.fromByteArray(bs.takeRight(4)))
    val aux = bs.drop(6).dropRight(4)
    (prefix, addressId, aux, height)
  }

  lazy val heightWithNumRW = KeyRW[(Height, TxNum)]

  def heightWithNum(h: Height, n: TxNum) = heightWithNumRW.toBytes((h, n))

  def parseHeightNum(bs: Array[Byte]) = heightWithNumRW.fromBytes(bs)

  def wavesBalanceHistory(addressId: AddressId): Key[Seq[Int]] = historyKey("waves-balance-history", 5, AddressId.toBytes(addressId))

  val WavesBalancePrefix: Short = 6
  def wavesBalance(addressId: Long)(height: Int): Key[Long] =
    Key("waves-balance", (WavesBalancePrefix, addressId, height), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  def assetBalanceLastHeight(addressId: Long, issueTxHeight: Height, issueTxNum: TxNum): Key[Int] =
    Key("asset-balance-last-height",
      (8.toShort, addressId, issueTxHeight, issueTxNum),
      Option(_).fold(0)(Ints.fromByteArray),
      Ints.toByteArray)

  val AssetBalancePrefix: Short = 9
  def assetBalance(addressId: AddressId, issueTxHeight: Height, issueTxNum: TxNum)(height: Int): Key[Long] = {
    val keyBytes = AssetBalancePrefix :: addressId :: issueTxHeight :: issueTxNum :: height :: HNil

    val balanceDecoder = (arr: Array[Byte]) => Option(arr).fold(0L)(Longs.fromByteArray)
    val balanceEncoder = (b: Long) => Longs.toByteArray(b)

    Key("asset-balance", keyBytes, balanceDecoder, balanceEncoder)
  }

  val AssetInfoPrefix: Short = 11

  def assetInfo(issueH: Height, issueN: TxNum)(height: Int): Key[AssetInfo] =
    Key("asset-info", (AssetInfoPrefix, (issueH, issueN), height), readAssetInfo, writeAssetInfo)

  def leaseBalanceHistory(addressId: AddressId): Key[Seq[Int]] =
    historyKey("lease-balance-history", 12, AddressId.toBytes(addressId))

  val LeaseBalancePrefix: Short = 13

  def leaseBalance(addressId: AddressId)(height: Int): Key[LeaseBalance] =
    Key("lease-balance", (LeaseBalancePrefix, addressId, height), readLeaseBalance, writeLeaseBalance)

  val LeaseStatusPrefix: Short = 15
  def leaseStatus(leaseId: ByteStr)(height: Int): Key[Boolean] =
    Key("lease-status", (LeaseStatusPrefix, leaseId.arr, height), _ (0) == 1, active => Array[Byte](if (active) 1 else 0))

  val FilledVolumeAndFeePrefix: Short = 17
  def filledVolumeAndFee(orderId: ByteStr)(height: Int): Key[VolumeAndFee] =
    Key("filled-volume-and-fee", (FilledVolumeAndFeePrefix, orderId.arr, height), readVolumeAndFee, writeVolumeAndFee)

  // 3, 10, 14, 16, 18, 22, 27, 33, 35, 37, 38, 41, 42, 44, 46 not used
  // 19, 20 were never used

  def changedAddresses(height: Int): Key[Seq[AddressId]] =
    Key("changed-addresses", (21.toShort, height), AddressId.readSeq, AddressId.writeSeq)

  def addressIdOfAlias(alias: Alias): Key[Option[AddressId]] =
    Key.opt("address-id-of-alias", (23.toShort, alias.bytes), AddressId.fromBytes, AddressId.toBytes)

  val lastAddressId: Key[Option[AddressId]] = Key.opt("last-address-id", 24.toShort, AddressId.fromBytes, AddressId.toBytes)

  def addressId(address: Address): Key[Option[AddressId]] = Key.opt("address-id", (25.toShort, address.bytes), AddressId.fromBytes, AddressId.toBytes)

  def idToAddress(id: AddressId): Key[Address] = Key("id-to-address", (26.toShort, id), Address.fromBytes(_).explicitGet(), _.bytes.arr)

  val AddressScriptPrefix: Short = 28
  def addressScript(addressId: AddressId)(height: Int): Key[Option[Script]] =
    Key.opt("address-script", (AddressScriptPrefix, addressId, height), ScriptReader.fromBytes(_).explicitGet(), _.bytes().arr)

  val approvedFeatures: Key[Map[Short, Int]]  = Key("approved-features", Array[Byte](0, 29), readFeatureMap, writeFeatureMap)
  val activatedFeatures: Key[Map[Short, Int]] = Key("activated-features", Array[Byte](0, 30), readFeatureMap, writeFeatureMap)

  def dataKeyChunkCount(addressId: AddressId): Key[Int] =
    Key("data-key-chunk-count", (31, addressId), Option(_).fold(0)(Ints.fromByteArray), Ints.toByteArray)
  def dataKeyChunk(addressId: AddressId, chunkNo: Int): Key[Seq[String]] =
    Key("data-key-chunk", (32.toShort, addressId, chunkNo), readStrings, writeStrings)

  val DataPrefix: Short = 34
  def data(addressId: AddressId, key: String)(height: Int): Key[Option[DataEntry[_]]] =
    Key.opt("data",
      (DataPrefix, addressId, key, height),
      DataEntry.parseValue(key, _, 0)._1,
      _.valueBytes)

  val SponsorshipPrefix: Short = 36

  def sponsorship(issueTxHeight: Height, issueTxNum: TxNum)(height: Int): Key[SponsorshipValue] =
    Key("sponsorship", (SponsorshipPrefix, (issueTxHeight, issueTxNum), height), readSponsorship, writeSponsorship)

  def addressesForWaves(seqNr: Int): Key[Seq[AddressId]] = Key("addresses-for-waves", (38.toShort, seqNr), AddressId.readSeq, AddressId.writeSeq)

  val AddressesForAssetPrefix: Short = 40

  def addressesForAsset(issueTxHeight: Height, issueTxNum: TxNum, addressId: AddressId): Key[AddressId] =
    Key("addresses-for-asset", (AddressesForAssetPrefix, issueTxHeight, issueTxNum, addressId), _ => addressId, _ => Array.emptyByteArray)

  val AliasIsDisabledPrefix: Short = 43
  def aliasIsDisabled(alias: Alias): Key[Boolean] =
    Key("alias-is-disabled", (AliasIsDisabledPrefix, alias.bytes.arr), Option(_).exists(_ (0) == 1), if (_) Array[Byte](1) else Array[Byte](0))

  /* 44: carryFeeHistory, obsolete */
  def carryFee(height: Int): Key[Long] = Key("carry-fee", (45.toShort, height), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  val AssetScriptPrefix: Short = 47

  def assetScript(issueTxHeight: Height, issueTxNum: TxNum)(height: Int): Key[Option[Script]] =
    Key.opt("asset-script", (AssetScriptPrefix, issueTxHeight, issueTxNum, height), ScriptReader.fromBytes(_).explicitGet(), _.bytes().arr)

  val safeRollbackHeight: Key[Int] = intKey("safe-rollback-height", 48)

  def changedDataKeys(height: Int, addressId: AddressId): Key[Seq[String]] =
    Key("changed-data-keys", (49.toShort, addressId, height), readStrings, writeStrings)

  val BlockHeaderPrefix: Short = 50

  def blockHeaderAndSizeAt(height: Height): Key[Option[(BlockHeader, Int)]] =
    Key.opt("block-header-at-height", (BlockHeaderPrefix, height), readBlockHeaderAndSize, writeBlockHeaderAndSize)

  def blockHeaderBytesAt(height: Height): Key[Option[Array[Byte]]] =
    Key.opt(
      "block-header-bytes-at-height",
      (BlockHeaderPrefix, height),
      _.drop(4),
      _ => throw new Exception("Key \"block-header-bytes-at-height\" - is read only!")
    )

  val TransactionInfoPrefix: Short = 51
  def transactionAt(height: Height, n: TxNum): Key[Option[Transaction]] =
    Key.opt(
      "nth-transaction-info-at-height",
      (TransactionInfoPrefix, height, n),
      data => TransactionParsers.parseBytes(data).get,
      _.bytes()
    )

  def transactionBytesAt(height: Height, n: TxNum): Key[Option[Array[Byte]]] =
    Key.opt(
      "nth-transaction-info-bytes-at-height",
      (TransactionInfoPrefix, height, n),
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
      (AddressTransactionHNPrefix, addressId, seqNr),
      readTransactionHNSeqAndType,
      writeTransactionHNSeqAndType
    )

  val TransactionHeightNumByIdPrefix: Short = 54
  def transactionHNById(txId: TransactionId): Key[Option[(Height, TxNum)]] =
    Key.opt(
      "transaction-height-and-nums-by-id",
      (TransactionHeightNumByIdPrefix, txId.arr),
      readTransactionHN,
      writeTransactionHN
    )

  val BlockTransactionsFeePrefix: Short = 55
  def blockTransactionsFee(height: Int): Key[Long] =
    Key(
      "block-transactions-fee",
      (BlockTransactionsFeePrefix, height),
      Longs.fromByteArray,
      Longs.toByteArray
    )

  val InvokeScriptResultPrefix: Short = 56
  def invokeScriptResult(height: Int, txNum: TxNum): Key[InvokeScriptResult] =
    Key("invoke-script-result", (InvokeScriptResultPrefix, height, txNum), InvokeScriptResult.fromBytes, InvokeScriptResult.toBytes)
}
