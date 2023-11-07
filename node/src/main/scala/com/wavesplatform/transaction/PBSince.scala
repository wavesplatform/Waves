package com.wavesplatform.transaction

import com.wavesplatform.transaction.PBSince.*

import scala.annotation.nowarn

sealed trait PBSince { self: Transaction & VersionedTransaction =>
  lazy val protobufVersion: TxVersion = this match {
    case _: V1 => TxVersion.V1
    case _: V2 => TxVersion.V2: @nowarn // scalac: unreachable code
    case _: V3 => TxVersion.V3
  }
  final def isProtobufVersion: Boolean = self.version >= protobufVersion
}

object PBSince {
  trait V1 extends PBSince { self: Transaction & VersionedTransaction => }
  trait V2 extends PBSince { self: Transaction & VersionedTransaction => }
  trait V3 extends PBSince { self: Transaction & VersionedTransaction => }
}
