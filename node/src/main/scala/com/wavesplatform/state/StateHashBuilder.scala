package com.wavesplatform.state

import java.nio.charset.StandardCharsets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.script.Script
import com.wavesplatform.serialization.Deser
import com.wavesplatform.state.StateHash.SectionId
import com.wavesplatform.state.StateHashBuilder.Result
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset

import scala.collection.mutable

object StateHashBuilder {
  case class Result(bySection: Map[SectionId.Value, ByteStr]) {
    def totalHash(prevHash: ByteStr): ByteStr = {
      val sortedHashes = bySection.toSeq.sortBy(_._1).map(_._2.arr)
      Bytes.concat(prevHash.arr +: sortedHashes: _*)
    }
  }

  private def createHash(bs: ByteStr): ByteStr =
    wavesplatform.crypto.fastHash(bs)
}

class StateHashBuilder {
  type Entry = Seq[ByteStr]
  case class Section(entries: Seq[Entry]) {
    def sortedEntries: Seq[Entry] = entries.sortBy {
      case e1 +: e2 +: _ => (e1, e2)
      case _             => ???
    }

    def payload: ByteStr = sortedEntries.foldLeft(ByteStr.empty) { case (bs, entry) => Bytes.concat((bs +: entry).map(_.arr): _*) }
  }

  // TODO should empty sections be included in total result?
  private[this] val map = mutable.AnyRefMap
    .empty[SectionId.Value, Section]
    .withDefaultValue(Section(Nil))

  private[this] def addEntry(id: SectionId.Value)(entry: ByteStr*): Unit = {
    val Section(entries) = map(id)
    map(id) = Section(entries :+ entry)
  }

  def addBalance(address: Address, asset: Asset, balance: Long): Unit = {
    addEntry(SectionId.Balance)(
      address.bytes,
      asset.byteRepr, // TODO is 0/1 prefix required?
      Longs.toByteArray(balance)
    )
  }

  def addDataEntry(address: Address, dataEntry: DataEntry[_]): Unit = {
    // TODO filter empty entries in master
    addEntry(SectionId.DataChanges)(
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
    addEntry(SectionId.SetScript)(
      address.bytes,
      Deser.serializeOption(script)(_.bytes())
    )
  }

  def addAssetScript(asset: IssuedAsset, script: Option[Script]): Unit = {
    addEntry(SectionId.SetAssetScript)(
      asset.id,
      Deser.serializeOption(script)(_.bytes())
    )
  }

  def addLease(leaseId: TransactionId, status: Boolean): Unit = {
    addEntry(SectionId.Lease)(
      leaseId,
      if (status) Array(1: Byte) else Array(0: Byte)
    )
  }

  def addSponsor(asset: IssuedAsset, minSponsoredFee: Long): Unit = {
    addEntry(SectionId.Sponsor)(
      asset.id,
      Longs.toByteArray(minSponsoredFee)
    )
  }

  def result(): Result = {
    val sectHashes =
      for ((id, section) <- this.map.toMap if section.entries.nonEmpty)
        yield id -> StateHashBuilder.createHash(section.payload)

    Result(sectHashes)
  }
}
