package com.wavesplatform.api

import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update

sealed abstract class UpdateType(name: String) {
  override val toString: String = name
}

object UpdateType {
  case object AppendBlock      extends UpdateType("b")
  case object AppendMicroBlock extends UpdateType("mb")
  case object Rollback         extends UpdateType("rb")
  case object Unknown          extends UpdateType("?")

  val All: List[UpdateType] = List(AppendBlock, AppendMicroBlock, Rollback, Unknown)

  def from(update: BlockchainUpdated.Update): UpdateType = update match {
    case Update.Append(append) =>
      append.body match {
        case Body.Empty         => Unknown
        case _: Body.Block      => AppendBlock
        case _: Body.MicroBlock => AppendMicroBlock
      }
    case _: Update.Rollback => Rollback
    case Update.Empty       => Unknown
  }
}
