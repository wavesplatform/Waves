package com.wavesplatform.blockchain

import com.wavesplatform.api.UpdateType
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.ride.runner.Processor
import com.wavesplatform.state.Height

class TestProcessor extends Processor {
  var actions: Vector[ProcessorAction] = Vector.empty

  override def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean] = None

  override def forceRollbackLiquid(): Unit = {}

  override def removeAllFrom(height: Height): Unit =
    actions = actions.appended(RemoveFrom(height))

  override def startScripts(): Unit = {}

  override def process(event: BlockchainUpdated): Unit =
    actions = actions.appended(Process(event))

  override def scheduleAffectedScripts(updateType: UpdateType): Unit =
    actions = actions.appended(RunAffectedScripts)

  override def toString: String = s"TestProcessor(${actions.mkString(", ")})"
}

sealed trait ProcessorAction extends Product with Serializable

case class RemoveFrom(height: Int) extends ProcessorAction

object RemoveFrom {
  def apply(event: SubscribeEvent): RemoveFrom = RemoveFrom(event.getUpdate.height)
  def next(event: SubscribeEvent): RemoveFrom  = RemoveFrom(event.getUpdate.height + 1)
}

case class Process(updated: BlockchainUpdated) extends ProcessorAction
object Process {
  def apply(event: SubscribeEvent): Process = Process(event.getUpdate)
}

case object RunAffectedScripts extends ProcessorAction
