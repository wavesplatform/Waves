package com.wavesplatform.account

import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto
import com.wavesplatform.test.PropSpec

class AccountSpecification extends PropSpec {

  property("Account.isValidAddress should return false for another address version") {
    forAll { (data: Array[Byte], AddressVersion2: Byte) =>
      val publicKeyHash   = crypto.secureHash(data).take(Address.HashLength)
      val withoutChecksum = AddressVersion2 +: AddressScheme.current.chainId +: publicKeyHash
      val addressVersion2 = Base58.encode(withoutChecksum ++ crypto.secureHash(withoutChecksum).take(Address.ChecksumLength))
      Address.fromString(addressVersion2).isRight shouldBe (AddressVersion2 == Address.AddressVersion)
    }
  }

  property("Weak public keys are detected correctly") {
    def decode(hexarr: Array[String]): Array[Array[Byte]] = hexarr.map(
      _.grouped(2).map(b => java.lang.Integer.parseInt(b, 16).toByte).toArray
    )

    val weakKeys = Array(
      "0000000000000000000000000000000000000000000000000000000000000000",
      "0000000000000000000000000000000000000000000000000000000000000080",
      "0100000000000000000000000000000000000000000000000000000000000000",
      "0100000000000000000000000000000000000000000000000000000000000080",
      "5f9c95bca3508c24b1d0b1559c83ef5b04445cc4581c8e86d8224eddd09f1157",
      "5f9c95bca3508c24b1d0b1559c83ef5b04445cc4581c8e86d8224eddd09f11d7",
      "e0eb7a7c3b41b8ae1656e3faf19fc46ada098deb9c32b1fd866205165f49b800",
      "e0eb7a7c3b41b8ae1656e3faf19fc46ada098deb9c32b1fd866205165f49b880",
      "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
      "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
      "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
      "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
      "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
      "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
    )
    decode(weakKeys).forall(crypto.isWeakPublicKey) shouldBe true

    val strongKeys = Array(
      "0000000000000000000000000000000000000000000000000000000000000100",
      "0000000000000000000000000000000000000000000000000000000000000001",
      "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20",
      "e0eb7a7c3b41b8ae1656e3faf19fc46ada098deb9c32b1fd866205165f49b801",
      "0400000000000000000000000000000000000000000000000000000000000000",
      "ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000",
      "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff03",
      "fffffffbfffffbffffdfffffdffffffffefffffefffff7fffff7ffffbfffff3f",
      "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff3f",
      "fffffffffeffff7ffffffffffeffff7ffffffffffeffff7ffffffffffeffff7f",
      "ebffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f"
    )

    decode(strongKeys).exists(crypto.isWeakPublicKey) shouldBe false
  }
}
