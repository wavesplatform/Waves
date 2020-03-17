package com.wavesplatform.state

import java.nio.charset.StandardCharsets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.StateHash.SectionId
import com.wavesplatform.state.StateHashBuilder.Result
import com.wavesplatform.transaction.Asset.IssuedAsset

import scala.collection.mutable

object StateHashBuilder {
  val EmptySectionHash: ByteStr = createSectionHash(ByteStr.empty)

  case class Result(bySection: Map[SectionId.Value, ByteStr]) {
    def totalHash(prevHash: ByteStr): ByteStr = {
      val sortedHashes = SectionId.values.toSeq.map(bySection.getOrElse(_, EmptySectionHash).arr)
      Bytes.concat(prevHash.arr +: sortedHashes: _*)
    }
  }

  private def createSectionHash(bs: ByteStr): ByteStr =
    crypto.fastHash(bs)
}

class StateHashBuilder {
  type Entry = Seq[ByteStr]
  case class Section(entries: Seq[Entry]) {
    def sortedEntries: Seq[Entry] = entries.sortBy(_.init)
    def payload: ByteStr          = sortedEntries.foldLeft(ByteStr.empty) { case (bs, entry) => Bytes.concat((bs +: entry).map(_.arr): _*) }
  }

  private[this] val map = mutable.AnyRefMap
    .empty[SectionId.Value, Section]
    .withDefaultValue(Section(Nil))

  private[this] def addEntry(id: SectionId.Value)(entry: ByteStr*): Unit = {
    val Section(entries) = map(id)
    map(id) = Section(entries :+ entry)
  }

  def addWavesBalance(address: Address, balance: Long): Unit = {
    addEntry(SectionId.WavesBalance)(
      address.bytes,
      Longs.toByteArray(balance)
    )
  }

  def addAssetBalance(address: Address, asset: IssuedAsset, balance: Long): Unit = {
    addEntry(SectionId.AssetBalance)(
      address.bytes,
      asset.id,
      Longs.toByteArray(balance)
    )
  }

  def addDataEntry(address: Address, dataEntry: DataEntry[_]): Unit = {
    addEntry(SectionId.DataEntry)(
      address.bytes,
      dataEntry.key.getBytes(StandardCharsets.UTF_8),
      dataEntry.valueBytes
    )
  }

  def addAlias(address: Address, alias: String): Unit = {
    addEntry(SectionId.Alias)(
      address.bytes,
      alias.getBytes(StandardCharsets.UTF_8)
    )
  }

  def addAccountScript(address: Address, script: Option[Script]): Unit = {
    addEntry(SectionId.AccountScript)(
      address.bytes,
      script.fold(ByteStr.empty)(_.bytes())
    )
  }

  def addAssetScript(asset: IssuedAsset, script: Option[Script]): Unit = {
    addEntry(SectionId.AssetScript)(
      asset.id,
      script.fold(ByteStr.empty)(_.bytes())
    )
  }

  def addLeaseBalance(address: Address, leaseIn: Long, leaseOut: Long): Unit = {
    addEntry(SectionId.LeaseBalance)(
      address.bytes,
      Longs.toByteArray(leaseIn),
      Longs.toByteArray(leaseOut)
    )
  }

  def addLeaseStatus(leaseId: TransactionId, status: Boolean): Unit = {
    addEntry(SectionId.LeaseStatus)(
      leaseId,
      if (status) Array(1: Byte) else Array(0: Byte)
    )
  }

  def addSponsor(asset: IssuedAsset, minSponsoredFee: Long): Unit = {
    addEntry(SectionId.Sponsorship)(
      asset.id,
      Longs.toByteArray(minSponsoredFee)
    )
  }

  def result(): Result = {
    val sectHashes =
      for ((id, section) <- this.map.toMap if section.entries.nonEmpty)
        yield id -> StateHashBuilder.createSectionHash(section.payload)

    Result(sectHashes)
  }
}
