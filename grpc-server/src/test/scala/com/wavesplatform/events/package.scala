package com.wavesplatform

import com.wavesplatform.events.protobuf.BlockchainUpdated as PBBlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.{Append, Update}
import com.wavesplatform.protobuf.transaction.SignedTransaction
import org.scalactic.source.Position
import org.scalatest.OptionValues.*
import org.scalatest.matchers.should.Matchers

package object events {
  implicit class BlockchainUpdatedExt(val se: PBBlockchainUpdated) extends Matchers {
    def append(implicit pos: Position): Append =
      se.update match {
        case Update.Append(append) => append
        case other                 => fail(s"${other.getClass.getSimpleName} is not an Append")
      }
  }

  implicit class AppendExt(val append: Append) extends Matchers {
    def transactionAt(idx: Int)(implicit pos: Position): SignedTransaction =
      append.body match {
        case Body.Empty             => fail("empty update body")
        case Body.Block(value)      => value.block.value.transactions.lift(idx).value
        case Body.MicroBlock(value) => value.microBlock.value.microBlock.value.transactions.lift(idx).value
      }
  }
}
