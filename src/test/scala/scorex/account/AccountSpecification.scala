package scorex.account

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.SecureCryptographicHash._

class AccountSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  property("Account.fromPublicKey should generate valid account") {
    forAll { data: Array[Byte] =>
      Account.isValid(Account.fromPublicKey(data)) shouldBe true
    }
  }

  property("Account.isValidAddress should return false for another address version") {
    forAll { (data: Array[Byte], AddressVersion2: Byte) =>
      val publicKeyHash = hash(data).take(Account.HashLength)
      val withoutChecksum = AddressVersion2 +: AddressScheme.current.chainId +: publicKeyHash
      val addressVersion2 = Base58.encode(withoutChecksum ++ hash(withoutChecksum).take(Account.ChecksumLength))
      Account.isValidAddress(addressVersion2) shouldBe (AddressVersion2 == Account.AddressVersion)
    }
  }
}

