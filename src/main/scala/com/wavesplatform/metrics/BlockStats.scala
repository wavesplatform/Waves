package com.wavesplatform.metrics

import java.net.InetSocketAddress

import com.wavesplatform.state2.ByteStr
import org.influxdb.dto.Point
import scorex.block.{Block, MicroBlock}

object BlockStats {

  private val StringIdLength = 6

  private sealed abstract class Event {
    val name: String = {
      val className = getClass.getName
      className.slice(className.lastIndexOf('$', className.length - 2) + 1, className.length - 1)
    }
  }

  private object Event {
    case object Received extends Event
    case object Applied extends Event
    case object Declined extends Event
    case object Mined extends Event
  }

  def received(b: Block, from: InetSocketAddress): Unit = write(
    block(b)
      .addField("from", from.toString)
      .addField("prop-time", System.currentTimeMillis() - b.timestamp)
      .addField("score", b.blockScore),
    Event.Received,
    Seq.empty
  )

  def applied(b: Block, height: Int): Unit = write(
    block(b)
      .addField("txs", b.transactionData.size)
      .addField("height", height),
    Event.Applied,
    Seq.empty
  )

  def declined(b: Block): Unit = write(
    block(b),
    Event.Declined,
    Seq.empty
  )

  def mined(b: Block, height: Int): Unit = write(
    block(b)
      .tag("parent-id", b.reference.toString.take(StringIdLength))
      .addField("txs", b.transactionData.size)
      .addField("score", b.blockScore)
      .addField("height", height),
    Event.Mined,
    Seq.empty
  )


  def received(m: MicroBlock, from: InetSocketAddress, propagationTime: Long): Unit = write(
    micro(m)
      .tag("parent-id", m.prevResBlockSig.toString.take(StringIdLength))
      .addField("from", from.toString)
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
      .addField("txs", m.transactionData.size),
    Event.Mined,
    Seq.empty
  )

  private def block(b: Block): Point.Builder = {
    Point
      .measurement("block")
      .tag("id", id(b.uniqueId))
  }

  private def micro(m: MicroBlock): Point.Builder = {
    Point
      .measurement("micro")
      .tag("id", id(m.uniqueId))
  }

  private def id(x: ByteStr): String = x.toString.take(StringIdLength)

  private def write(init: Point.Builder, event: Event, addFields: Seq[(String, String)]): Unit = {
    Metrics.write(addFields.foldLeft(init.tag("event", event.name)) { case (r, (k, v)) => r.addField(k, v) })
  }
}
