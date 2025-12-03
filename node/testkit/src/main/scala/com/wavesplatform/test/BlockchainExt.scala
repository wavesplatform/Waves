package com.wavesplatform.test

import com.wavesplatform.account.Address
import com.wavesplatform.state.{Blockchain, IntegerDataEntry, StringDataEntry}

object BlockchainExt {
  extension (b: Blockchain) {
    def integerData(acc: Address, key: String): Option[Long] = b.accountData(acc, key).collect { case ida: IntegerDataEntry =>
      ida.value
    }

    def stringData(acc: Address, key: String): Option[String] = b.accountData(acc, key).collect { case sda: StringDataEntry =>
      sda.value
    }
  }
}
