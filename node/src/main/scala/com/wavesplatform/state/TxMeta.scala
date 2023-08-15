package com.wavesplatform.state

import com.wavesplatform.database.protobuf.Status as PBStatus
import com.wavesplatform.state.TxMeta.Status

case class TxMeta(height: Height, status: Status, spentComplexity: Long)

object TxMeta {
  sealed trait Status {
    def protobuf: PBStatus
  }
  object Status {
    case object Failed extends Status {
      override def protobuf: PBStatus = PBStatus.FAILED
    }
    case object Succeeded extends Status {
      override def protobuf: PBStatus = PBStatus.SUCCEEDED
    }
    case object Elided extends Status {
      override def protobuf: PBStatus = PBStatus.ELIDED
    }

    def fromProtobuf(status: PBStatus): Status =
      status match {
        case PBStatus.FAILED    => Failed
        case PBStatus.SUCCEEDED => Succeeded
        case _                  => Elided
      }
  }
}
