package scorex.account

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.SecureCryptographicHash._

class AccountSpecification extends PropSpec with PropertyChecks with Matchers {

  property("Account.isValidAddress should return false for another address version") {
    forAll { (data: Array[Byte], AddressVersion2: Byte) =>
      val publicKeyHash = hash(data).take(Address.HashLength)
      val withoutChecksum = AddressVersion2 +: AddressScheme.current.chainId +: publicKeyHash
      val addressVersion2 = Base58.encode(withoutChecksum ++ hash(withoutChecksum).take(Address.ChecksumLength))
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

