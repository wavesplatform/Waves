package com.wavesplatform.database

import com.google.common.base.Charsets.UTF_8
import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.state._
import scorex.account.{Address, Alias}
import scorex.block.{Block, BlockHeader}
import scorex.transaction.Transaction
import scorex.transaction.smart.script.{Script, ScriptReader}

object Keys {
  import KeyHelpers._

  val version: Key[Int]               = intKey(0, default = 1)
  val height: Key[Int]                = intKey(1)
  def score(height: Int): Key[BigInt] = Key(h(2, height), Option(_).fold(BigInt(0))(BigInt(_)), _.toByteArray)

  private def blockAtHeight(height: Int) = h(3, height)

  def blockAt(height: Int): Key[Option[Block]]          = Key.opt[Block](blockAtHeight(height), Block.parseBytes(_).get, _.bytes())
  def blockBytes(height: Int): Key[Option[Array[Byte]]] = Key.opt[Array[Byte]](blockAtHeight(height), identity, identity)
  def blockHeader(height: Int): Key[Option[(BlockHeader, Int)]] =
    Key.opt[(BlockHeader, Int)](blockAtHeight(height), b => (BlockHeader.parseBytes(b).get._1, b.length), unsupported("Can't write block headers")) // this dummy encoder is never used: we only store blocks, not block headers

  def heightOf(blockId: ByteStr): Key[Option[Int]] = Key.opt[Int](hash(4, blockId), Ints.fromByteArray, Ints.toByteArray)

  def wavesBalanceHistory(addressId: BigInt): Key[Seq[Int]] = historyKey(5, addressId.toByteArray)
  def wavesBalance(addressId: BigInt)(height: Int): Key[Long] =
    Key(hAddr(6, height, addressId), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  def assetList(addressId: BigInt): Key[Set[ByteStr]]                         = Key(addr(7, addressId), readTxIds(_).toSet, assets => writeTxIds(assets.toSeq))
  def assetBalanceHistory(addressId: BigInt, assetId: ByteStr): Key[Seq[Int]] = historyKey(8, addressId.toByteArray ++ assetId.arr)
  def assetBalance(addressId: BigInt, assetId: ByteStr)(height: Int): Key[Long] =
    Key(hBytes(9, height, addressId.toByteArray ++ assetId.arr), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  def assetInfoHistory(assetId: ByteStr): Key[Seq[Int]]        = historyKey(10, assetId.arr)
  def assetInfo(assetId: ByteStr)(height: Int): Key[AssetInfo] = Key(hBytes(11, height, assetId.arr), readAssetInfo, writeAssetInfo)

  def leaseBalanceHistory(addressId: BigInt): Key[Seq[Int]] = historyKey(12, addressId.toByteArray)
  def leaseBalance(addressId: BigInt)(height: Int): Key[LeaseBalance] =
    Key(hAddr(13, height, addressId), readLeaseBalance, writeLeaseBalance)
  def leaseStatusHistory(leaseId: ByteStr): Key[Seq[Int]] = historyKey(14, leaseId.arr)
  def leaseStatus(leaseId: ByteStr)(height: Int): Key[Boolean] =
    Key(hBytes(15, height, leaseId.arr), _(0) == 1, active => Array[Byte](if (active) 1 else 0))

  def filledVolumeAndFeeHistory(orderId: ByteStr): Key[Seq[Int]]           = historyKey(16, orderId.arr)
  def filledVolumeAndFee(orderId: ByteStr)(height: Int): Key[VolumeAndFee] = Key(hBytes(17, height, orderId.arr), readVolumeAndFee, writeVolumeAndFee)

  def transactionInfo(txId: ByteStr): Key[Option[(Int, Transaction)]] = Key.opt(hash(18, txId), readTransactionInfo, writeTransactionInfo)
  def transactionHeight(txId: ByteStr): Key[Option[Int]] =
    Key.opt(hash(18, txId), readTransactionHeight, unsupported("Can't write transaction height only"))

  // 19: address transaction history (was never used, actually)
  def addressTransactionIdsAtHeight(height: Int, addressId: BigInt): Key[Seq[(Int, ByteStr)]] =
    Key(hAddr(20, height, addressId), readTransactionIds, writeTransactionIds)

  def changedAddresses(height: Int): Key[Seq[BigInt]] = Key(h(21, height), readBigIntSeq, writeBigIntSeq)

  def transactionIdsAtHeight(height: Int): Key[Seq[ByteStr]] = Key(h(22, height), readTxIds, writeTxIds)

  def addressIdOfAlias(alias: Alias): Key[Option[BigInt]] = Key.opt(bytes(23, alias.bytes.arr), BigInt(_), _.toByteArray)

  val lastAddressId: Key[Option[BigInt]] = Key.opt(Array[Byte](0, 24), BigInt(_), _.toByteArray)

  def addressId(address: Address): Key[Option[BigInt]] = Key.opt(bytes(25, address.bytes.arr), BigInt(_), _.toByteArray)
  def idToAddress(id: BigInt): Key[Address]            = Key(bytes(26, id.toByteArray), Address.fromBytes(_).explicitGet(), _.bytes.arr)

  def addressScriptHistory(addressId: BigInt): Key[Seq[Int]] = historyKey(27, addressId.toByteArray)
  def addressScript(addressId: BigInt)(height: Int): Key[Option[Script]] =
    Key.opt(hAddr(28, height, addressId), ScriptReader.fromBytes(_).explicitGet(), _.bytes().arr)

  def approvedFeatures: Key[Map[Short, Int]]  = Key(Array[Byte](0, 29), readFeatureMap, writeFeatureMap)
  def activatedFeatures: Key[Map[Short, Int]] = Key(Array[Byte](0, 30), readFeatureMap, writeFeatureMap)

  def dataKeyChunkCount(addressId: BigInt): Key[Int] = Key(addr(31, addressId), Option(_).fold(0)(Ints.fromByteArray), Ints.toByteArray)
  def dataKeyChunk(addressId: BigInt, chunkNo: Int): Key[Seq[String]] =
    Key(addr(32, addressId) ++ Ints.toByteArray(chunkNo), readStrings, writeStrings)

  def dataHistory(addressId: BigInt, key: String): Key[Seq[Int]] = historyKey(33, addressId.toByteArray ++ key.getBytes(UTF_8))
  def data(addressId: BigInt, key: String)(height: Int): Key[Option[DataEntry[_]]] =
    Key.opt(hBytes(34, height, addressId.toByteArray ++ key.getBytes(UTF_8)), DataEntry.parseValue(key, _, 0)._1, _.valueBytes)

  def sponsorshipHistory(assetId: ByteStr): Key[Seq[Int]]               = historyKey(35, assetId.arr)
  def sponsorship(assetId: ByteStr)(height: Int): Key[SponsorshipValue] = Key(hBytes(36, height, assetId.arr), readSponsorship, writeSponsorship)

  val addressesForWavesSeqNr: Key[Int]                = intKey(37)
  def addressesForWaves(seqNr: Int): Key[Seq[BigInt]] = Key(h(38, seqNr), readBigIntSeq, writeBigIntSeq)

  def addressesForAssetSeqNr(assetId: ByteStr): Key[Int]                = bytesSeqNr(39, assetId.arr)
  def addressesForAsset(assetId: ByteStr, seqNr: Int): Key[Seq[BigInt]] = Key(hBytes(40, seqNr, assetId.arr), readBigIntSeq, writeBigIntSeq)

  def addressTransactionSeqNr(addressId: BigInt): Key[Int] = bytesSeqNr(41, addressId.toByteArray)
  def addressTransactionIds(addressId: BigInt, seqNr: Int): Key[Seq[(Int, ByteStr)]] =
    Key(hBytes(42, seqNr, addressId.toByteArray), readTransactionIds, writeTransactionIds)

  val AliasIsDisabledPrefix: Short = 43
  def aliasIsDisabled(alias: Alias): Key[Boolean] =
    Key(bytes(AliasIsDisabledPrefix, alias.bytes.arr), Option(_).exists(_(0) == 1), if (_) Array[Byte](1) else Array[Byte](0))
}
