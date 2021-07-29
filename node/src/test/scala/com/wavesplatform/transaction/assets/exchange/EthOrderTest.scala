package com.wavesplatform.transaction.assets.exchange

import com.wavesplatform.account.{AddressScheme, EthereumAddress, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.test.FlatSpec
import com.wavesplatform.transaction.Asset.IssuedAsset
import org.bouncycastle.util.encoders.Hex
import org.scalatest.BeforeAndAfterAll

class EthOrderTest extends FlatSpec with BeforeAndAfterAll {
  private[this] val StubBytes32 = Array.fill(32)('E'.toByte)

  "ETH signed order" should "recover signer public key correctly" in {
    val testOrder = Order(
      Order.V1,
      PublicKey(StubBytes32),
      PublicKey(StubBytes32),
      AssetPair(IssuedAsset(ByteStr(StubBytes32)), IssuedAsset(ByteStr(StubBytes32))),
      OrderType.BUY,
      1,
      1,
      123,
      321,
      1,
      IssuedAsset(ByteStr(StubBytes32))
    )

    val signature =
      Hex.decode("b557dae4c614146dd35ba6fd80e4702a75d33ffcb8af09e80e0c1a7386b8ffcb5b76bd8037f6484de809a80a5b39a224301c76e8bad9b1a9e7ada53ba6fa7e361c")

    val result = EthOrders.recoverEthSignerKey(testOrder, signature)
    result.toEthAddress shouldBe EthereumAddress("0x8E1557d6AdE477b98F5Dd7a2a50Ae0513c0C9088")
  }

  override protected def beforeAll(): Unit = {
    AddressScheme.current = new AddressScheme {
      val chainId: Byte = 'E'.toByte
    }
    super.beforeAll()
  }

  override protected def afterAll(): Unit = {
    AddressScheme.current = new AddressScheme {
      val chainId: Byte = 'T'.toByte
    }
    super.afterAll()
  }
}
