package com.wavesplatform.metrics

import org.influxdb.dto.Point
import scorex.block.{Block, MicroBlock}

object BlockStats {

  sealed abstract class Event {
    val name: String = {
      val className = getClass.getName
      className.slice(className.lastIndexOf('$', className.length - 2) + 1, className.length - 1)
    }
  }

  object Event {
    case object Received extends Event
    case object Applied extends Event
    case object Rejected extends Event
    case object Mined extends Event
  }

  def write(b: Block, event: Event, addFields: (String, String)*): Unit = write(
    Point
      .measurement("block")
      .addField("id", b.uniqueId.toString),
    event,
    addFields
  )

  def write(m: MicroBlock, event: Event, addFields: (String, String)*): Unit = write(
    Point
      .measurement("micro")
      .addField("id", m.uniqueId.toString),
    event,
    addFields
  )

  private def write(init: Point.Builder, event: Event, addFields: Seq[(String, String)]): Unit = {
    Metrics.write(addFields.foldLeft(init.addField("event", event.name)) { case (r, (k, v)) => r.addField(k, v) })
  }
}
