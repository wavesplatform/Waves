package com.wavesplatform.blockchain

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.riderunner.Processor
import com.wavesplatform.state.Height
import monix.eval.Task

class TestProcessor extends Processor {
  var actions: Vector[ProcessorAction] = Vector.empty

  override def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean] = None

  /** Includes removeBlocksFrom
    */
  override def forceRollbackLiquid(): Unit = {}

  override def removeAllFrom(height: Height): Unit = {
    actions = actions.appended(RemoveFrom(height))
  }

  override def process(event: BlockchainUpdated): Unit = {
    actions = actions.appended(Process(event))
  }

  override def runAffectedScripts(): Task[Unit] = {
    actions = actions.appended(RunAffectedScripts)
    Task.now(())
  }

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
