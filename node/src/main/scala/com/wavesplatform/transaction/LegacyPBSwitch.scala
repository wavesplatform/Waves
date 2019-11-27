package com.wavesplatform.transaction

import com.wavesplatform.protobuf.transaction.PBTransactions

trait LegacyPBSwitch { self: Transaction with VersionedTransaction =>
  def protobufVersion: TxVersion
  final def isProtobufVersion: Boolean = self.version >= protobufVersion

  override def bytesSize: Int =
    if (isProtobufVersion) PBTransactions.protobuf(self).serializedSize else bytes().length
}

object LegacyPBSwitch {
  trait V2 extends LegacyPBSwitch { self: Transaction with VersionedTransaction =>
    override def protobufVersion: TxVersion = TxVersion.V2
  }

  trait V3 extends LegacyPBSwitch { self: Transaction with VersionedTransaction =>
    override def protobufVersion: TxVersion = TxVersion.V3
  }
}
