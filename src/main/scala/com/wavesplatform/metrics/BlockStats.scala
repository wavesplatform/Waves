package com.wavesplatform.metrics

import com.wavesplatform.network.{HandshakeHandler, MicroBlockInv}
import com.wavesplatform.state2.ByteStr
import io.netty.channel.ChannelHandlerContext
import org.influxdb.dto.Point
import scorex.block.{Block, MicroBlock}

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
    case object Inv extends Event
    case object Received extends Event
    case object Applied extends Event
    case object Declined extends Event
    case object Mined extends Event
  }

  private sealed abstract class Type extends Named
  private object Type {
    case object Block extends Type
    case object Micro extends Type
  }

  sealed abstract class BlockType extends Named
  object BlockType {
    case object Broadcast extends BlockType
    case object Ext extends BlockType
  }

  def received(b: Block, bType: BlockType, ctx: ChannelHandlerContext): Unit = write(
    block(b, bType)
      .addField("from", nodeName(ctx))
      .addField("prop-time", System.currentTimeMillis() - b.timestamp)
      .addField("score", b.blockScore)
      .addField("bt", b.consensusData.baseTarget),
    Event.Received,
    Seq.empty
  )

  def applied(b: Block, bType: BlockType, height: Int): Unit = write(
    block(b, bType)
      .addField("txs", b.transactionData.size)
      .addField("height", height),
    Event.Applied,
    Seq.empty
  )

  def declined(b: Block, bType: BlockType): Unit = write(
    block(b, bType),
    Event.Declined,
    Seq.empty
  )

  def mined(b: Block, height: Int): Unit = write(
    block(b, BlockType.Broadcast)
      .tag("parent-id", id(b.reference))
      .addField("txs", b.transactionData.size)
      .addField("score", b.blockScore)
      .addField("bt", b.consensusData.baseTarget)
      .addField("height", height),
    Event.Mined,
    Seq.empty
  )


  def inv(m: MicroBlockInv, ctx: ChannelHandlerContext): Unit = write(
    measurement(Type.Micro)
      .tag("id", id(m.totalBlockSig))
      .tag("parent-id", id(m.prevBlockSig))
      .addField("from", nodeName(ctx)),
    Event.Inv,
    Seq.empty
  )

  def received(m: MicroBlock, ctx: ChannelHandlerContext, propagationTime: Long): Unit = write(
    micro(m)
      .tag("parent-id", id(m.prevResBlockSig))
      .addField("from", nodeName(ctx))
      .addField("prop-time", propagationTime),
    Event.Received,
    Seq.empty
  )

  def applied(m: MicroBlock): Unit = write(
    micro(m)
      .addField("txs", m.transactionData.size),
    Event.Applied,
    Seq.empty
  )

  def declined(m: MicroBlock): Unit = write(
    micro(m),
    Event.Declined,
    Seq.empty
  )

  def mined(m: MicroBlock): Unit = write(
    micro(m)
      .tag("parent-id", id(m.prevResBlockSig))
      .addField("txs", m.transactionData.size),
    Event.Mined,
    Seq.empty
  )

  private def block(b: Block, bType: BlockType): Point.Builder = measurement(Type.Block)
    .tag("id", id(b.uniqueId))
    .tag("btype", bType.name)

  private def micro(m: MicroBlock): Point.Builder = measurement(Type.Micro)
    .tag("id", id(m.totalResBlockSig))

  private def measurement(t: Type): Point.Builder = Point.measurement("block").tag("type", t.toString)

  private def nodeName(ctx: ChannelHandlerContext): String = {
    Option(ctx.channel().attr(HandshakeHandler.NodeNameAttributeKey).get()).getOrElse("")
  }

  private def id(x: ByteStr): String = x.toString.take(StringIdLength)

  private def write(init: Point.Builder, event: Event, addFields: Seq[(String, String)]): Unit = {
    Metrics.write(addFields.foldLeft(init.tag("event", event.name)) { case (r, (k, v)) => r.addField(k, v) })
  }
}
