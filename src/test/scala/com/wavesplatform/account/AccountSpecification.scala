package com.wavesplatform.account

import com.wavesplatform.crypto
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.wavesplatform.utils.Base58

class AccountSpecification extends PropSpec with PropertyChecks with Matchers {

  property("Account.isValidAddress should return false for another address version") {
    forAll { (data: Array[Byte], AddressVersion2: Byte) =>
      val publicKeyHash   = crypto.secureHash(data).take(Address.HashLength)
      val withoutChecksum = AddressVersion2 +: AddressScheme.current.chainId +: publicKeyHash
      val addressVersion2 = Base58.encode(withoutChecksum ++ crypto.secureHash(withoutChecksum).take(Address.ChecksumLength))
      Address.fromString(addressVersion2).isRight shouldBe (AddressVersion2 == Address.AddressVersion)
    }
  }

  property("PublicKeyAccount should return Address as it's string representation") {
    forAll { bytes: Array[Byte] =>
      val a = PublicKeyAccount.apply(bytes)
      a.toString shouldBe a.toAddress.address
    }
  }
}
