package com.wavesplatform.state

import java.nio.charset.StandardCharsets

import com.google.common.primitives.Longs
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.StateHash.SectionId
import com.wavesplatform.state.StateHashBuilder.Result
import com.wavesplatform.transaction.Asset.IssuedAsset
import org.bouncycastle.crypto.digests.Blake2bDigest

import scala.collection.mutable

object StateHashBuilder {
  val EmptySectionHash: ByteStr = createSectionHash(Nil)

  final case class Result(hashes: Map[SectionId.Value, ByteStr]) {
    def createStateHash(prevHash: ByteStr): StateHash = {
      val sortedHashes = SectionId.values.toSeq.map(hashes.getOrElse(_, EmptySectionHash))
      val payload      = prevHash +: sortedHashes
      StateHash(createSectionHash(payload), hashes)
    }
  }

  private def createSectionHash(bs: Iterable[ByteStr], digestFn: Blake2bDigest = newDigestInstance()): ByteStr = {
    bs.foreach(bs => digestFn.update(bs.arr, 0, bs.arr.length))
    val result = new Array[Byte](crypto.DigestLength)
    digestFn.doFinal(result, 0)
    ByteStr(result)
  }

  private def newDigestInstance(): Blake2bDigest = new Blake2bDigest(crypto.DigestLength * 8)
}

class StateHashBuilder {
  import com.wavesplatform.utils.byteStrOrdering
  private val maps = Vector.fill(SectionId.maxId)(mutable.TreeMap.empty[ByteStr, Array[Byte]])

  private def addEntry(section: SectionId.Value, key: Array[Byte]*)(value: Array[Byte]*): Unit = {
    val solidKey   = ByteStr(key.reduce(_ ++ _))
    val solidValue = value.foldLeft(Array.emptyByteArray)(_ ++ _)
    maps(section.id)(solidKey) = solidValue
  }

  def addWavesBalance(address: Address, balance: Long): Unit = {
    addEntry(SectionId.WavesBalance, address.bytes)(Longs.toByteArray(balance))
  }

  def addAssetBalance(address: Address, asset: IssuedAsset, balance: Long): Unit = {
    addEntry(SectionId.AssetBalance, address.bytes, asset.id.arr)(
      Longs.toByteArray(balance)
    )
  }

  def addDataEntry(address: Address, dataEntry: DataEntry[?]): Unit = {
    addEntry(SectionId.DataEntry, address.bytes, dataEntry.key.getBytes(StandardCharsets.UTF_8))(
      dataEntry.valueBytes
    )
  }

  def addAlias(address: Address, alias: String): Unit = {
    addEntry(SectionId.Alias, address.bytes, alias.getBytes(StandardCharsets.UTF_8))()
  }

  def addAccountScript(address: Address, script: Option[Script]): Unit = {
    addEntry(SectionId.AccountScript, address.bytes)(
      script.fold(Array.emptyByteArray)(_.bytes().arr)
    )
  }

  def addAssetScript(asset: IssuedAsset, script: Option[Script]): Unit = {
    addEntry(SectionId.AssetScript, asset.id.arr)(
      script.fold(Array.emptyByteArray)(_.bytes().arr)
    )
  }

  def addLeaseBalance(address: Address, leaseIn: Long, leaseOut: Long): Unit = {
    addEntry(SectionId.LeaseBalance, address.bytes)(
      Longs.toByteArray(leaseIn),
      Longs.toByteArray(leaseOut)
    )
  }

  def addLeaseStatus(leaseId: ByteStr, isActive: Boolean): Unit = {
    addEntry(SectionId.LeaseStatus, leaseId.arr)(
      if (isActive) Array(1: Byte) else Array(0: Byte)
    )
  }

  def addSponsorship(asset: IssuedAsset, minSponsoredFee: Long): Unit = {
    addEntry(SectionId.Sponsorship, asset.id.arr)(
      Longs.toByteArray(minSponsoredFee)
    )
  }

  def result(): Result = {
    val digestInstance = StateHashBuilder.newDigestInstance()
    val sectHashes = for {
      (section, id) <- this.maps.zipWithIndex if section.nonEmpty
    } yield SectionId(id) -> StateHashBuilder.createSectionHash(section.flatMap { case (k, v) => Seq(k, ByteStr(v)) }, digestInstance)

    Result(sectHashes.toMap)
  }
}
