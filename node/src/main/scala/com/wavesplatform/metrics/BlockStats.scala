package com.wavesplatform.metrics

import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.network.{BlockSnapshot, HandshakeHandler, MicroBlockInv, MicroBlockSnapshot}
import io.netty.channel.Channel
import org.influxdb.dto.Point

object BlockStats {

  private val StringIdLength = 6

  trait Named {
    private[BlockStats] val name: String = {
      val className = getClass.getName
      className.slice(className.lastIndexOf('$', className.length - 2) + 1, className.length - 1)
    }
  }

  private sealed abstract class Event extends Named

  private object Event {
    case object Inv      extends Event
    case object Received extends Event
    case object Applied  extends Event
    case object Appended extends Event
    case object Declined extends Event
    case object Mined    extends Event
  }

  private sealed abstract class Type extends Named

  private object Type {
    case object Block              extends Type
    case object Micro              extends Type
    case object BlockSnapshot      extends Type
    case object MicroBlockSnapshot extends Type
  }

  sealed abstract class Source extends Named

  object Source {
    case object Broadcast extends Source
    case object Ext       extends Source
  }

  def received(b: Block, source: Source, ch: Channel): Unit = write(
    block(b, source)
      .addField("from", nodeName(ch))
      .addField("bt", b.header.baseTarget),
    Event.Received,
    Seq.empty
  )

  def received(s: BlockSnapshot, source: Source, ch: Channel): Unit = write(
    blockSnapshot(s, source)
      .addField("from", nodeName(ch)),
    Event.Received,
    Seq.empty
  )

  def applied(b: Block, source: Source, newHeight: Int): Unit = write(
    block(b, source)
      .addField("txs", b.transactionData.size)
      .addField("height", newHeight),
    Event.Applied,
    Seq.empty
  )

  def declined(b: Block, source: Source): Unit = write(
    block(b, source),
    Event.Declined,
    Seq.empty
  )

  def mined(b: Block, baseHeight: Int): Unit = write(
    block(b, Source.Broadcast)
      .tag("parent-id", id(b.header.reference))
      .addField("txs", b.transactionData.size)
      .addField("bt", b.header.baseTarget)
      .addField("height", baseHeight),
    Event.Mined,
    Seq.empty
  )

  def appended(b: Block, complexity: Long): Unit = write(
    measurement(Type.Block)
      .tag("id", id(b.id()))
      .addField("complexity", complexity),
    Event.Appended,
    Seq.empty
  )

  def inv(m: MicroBlockInv, ch: Channel): Unit = write(
    measurement(Type.Micro)
      .tag("id", id(m.totalBlockId))
      .tag("parent-id", id(m.reference))
      .addField("from", nodeName(ch)),
    Event.Inv,
    Seq.empty
  )

  def received(m: MicroBlockSnapshot, ch: Channel, blockId: BlockId): Unit = write(
    microBlockSnapshot(blockId)
      .addField("from", nodeName(ch)),
    Event.Received,
    Seq.empty
  )

  def received(m: MicroBlock, ch: Channel, blockId: BlockId): Unit = write(
    micro(blockId)
      .tag("parent-id", id(m.reference))
      .addField("from", nodeName(ch)),
    Event.Received,
    Seq.empty
  )

  def applied(m: MicroBlock, blockId: BlockId): Unit = write(
    micro(blockId)
      .addField("txs", m.transactionData.size),
    Event.Applied,
    Seq.empty
  )

  def declined(blockId: BlockId): Unit = write(
    micro(blockId),
    Event.Declined,
    Seq.empty
  )

  def mined(m: MicroBlock, blockId: BlockId): Unit = write(
    micro(blockId)
      .tag("parent-id", id(m.reference))
      .addField("txs", m.transactionData.size),
    Event.Mined,
    Seq.empty
  )

  private def block(b: Block, source: Source): Point.Builder = {
    val isWhitelistMiner = {
      val whitelistAddrs = Set(
        "3P2HNUd5VUPLMQkJmctTPEeeHumiPN2GkTb",
        "3PA1KvFfq9VuJjg45p2ytGgaNjrgnLSgf4r",
        "3P9DEDP5VbyXQyKtXDUt2crRPn5B7gs6ujc",
        "3P23fi1qfVw6RVDn4CH2a5nNouEtWNQ4THs",
        "3PEDjPSkKrMtaaJJLGfL849Fg39TSZ7WGzY",
        "3P5dg6PtSAQmdH1qCGKJWu7bkzRG27mny5i",
        "3PNDoRLsFoPtW1P3nvVHAt7V6hfpyQ8Az9w"
      )
      whitelistAddrs(b.sender.toAddress.toString)
    }

    measurement(Type.Block)
      .tag("id", id(b.id()))
      .tag("source", source.name)
      .tag("whitelist", isWhitelistMiner.toString)
  }

  private def blockSnapshot(s: BlockSnapshot, source: Source): Point.Builder = {
    measurement(Type.BlockSnapshot)
      .tag("id", id(s.blockId))
      .tag("source", source.name)
  }

  private def microBlockSnapshot(totalBlockId: BlockId): Point.Builder =
    measurement(Type.MicroBlockSnapshot)
      .tag("id", id(totalBlockId))

  private def micro(blockId: BlockId): Point.Builder =
    measurement(Type.Micro)
      .tag("id", id(blockId))

  private def measurement(t: Type): Point.Builder =
    Point.measurement("block").tag("type", t.toString)

  private def nodeName(ch: Channel): String =
    if (ch == null) "???" else Option(ch.attr(HandshakeHandler.NodeNameAttributeKey).get()).getOrElse("")

  def id(x: ByteStr): String = x.toString.take(StringIdLength)

  private def write(init: Point.Builder, event: Event, addFields: Seq[(String, String)]): Unit = {
    Metrics.write(addFields.foldLeft(init.tag("event", event.name)) { case (r, (k, v)) => r.addField(k, v) })
  }
}
