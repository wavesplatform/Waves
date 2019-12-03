package com.wavesplatform.transaction

trait LegacyPBSwitch { self: VersionedTransaction =>
  def protobufVersion: TxVersion
  def lastVersion: TxVersion = protobufVersion
  def isProtobufVersion: Boolean = self.version >= protobufVersion
}

object LegacyPBSwitch {
  trait V2 extends LegacyPBSwitch { self: VersionedTransaction =>
    override def protobufVersion: TxVersion = TxVersion.V2
  }

  trait V3 extends LegacyPBSwitch { self: VersionedTransaction =>
    override def protobufVersion: TxVersion = TxVersion.V3
  }
}
