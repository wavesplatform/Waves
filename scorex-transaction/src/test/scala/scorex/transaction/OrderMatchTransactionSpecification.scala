package scorex.transaction

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.PrivateKeyAccount
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.assets.exchange._
import scorex.utils._

class OrderMatchTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  /*
    property("OrderMatch transaction serialization roundtrip") {
      forAll(orderMatchGenerator) { om: OrderMatch =>
        val recovered = Order.parseBytes(om.bytes).get
        recovered.bytes shouldEqual om.bytes
      }
    }

    property("OrderMatch generator should generate valid orders") {
      forAll(orderMatchGenerator) { om: OrderMatch =>
        om.isValid(Seq()) shouldBe true
      }
    }

    property("OrderMatch validation") {
      forAll(orderMatchGenerator) { om: OrderMatch =>
        om.isValid(Seq()) shouldBe true
        om.isValid(Seq(om)) shouldBe false
        //TODO incorrect price/matcherFee/matcherSignature/NonMatched orders/ amount with partial match
      }
    }
  */

  property("OrderMatch balance changes") {
    forAll { (s1: Array[Byte], s2: Array[Byte], s3: Array[Byte]) =>
      val sender1 = new PrivateKeyAccount(s1)
      val sender2 = new PrivateKeyAccount(s2)
      val matcher = new PrivateKeyAccount(s3)
      val spendAssetId = randomBytes(32)
      val receiveAssetId = randomBytes(32)
      val priceAssetId = if (ByteArray.compare(spendAssetId, receiveAssetId) > 0) receiveAssetId else spendAssetId
      val amountAssetId = if (ByteArray.compare(spendAssetId, receiveAssetId) <= 0) spendAssetId else receiveAssetId
      val maxtTime = NTP.correctedTime() + Order.MaxLiveTime
      val buyPrice = 60
      val sellPrice = 50
      val buyAmount = 2
      val sellAmount = 3
      val mf1 = 1
      val mf2 = 2

      val o1 = Order(sender1, matcher, priceAssetId, amountAssetId, buyPrice, buyAmount, maxtTime, mf1)
      val o2 = Order(sender2, matcher, amountAssetId, priceAssetId, sellPrice, sellAmount, maxtTime, mf2)

      val unsigned = OrderMatch(o1, o2, sellPrice, buyAmount, mf1 + mf2, 1, maxtTime - Order.MaxLiveTime, Array())

      signed(unsigned, matcher).isValid(Seq()) shouldBe true
      signed(unsigned.copy(price = sellPrice + 1), matcher).isValid(Seq()) shouldBe false
      signed(unsigned.copy(price = buyPrice), matcher).isValid(Seq()) shouldBe true
      signed(unsigned.copy(amount = buyAmount + 1), matcher).isValid(Seq()) shouldBe false
      signed(unsigned.copy(amount = buyAmount - 1), matcher).isValid(Seq()) shouldBe true
      signed(unsigned.copy(matcherFee = mf1 + mf2 + 1), matcher).isValid(Seq()) shouldBe false
      signed(unsigned.copy(matcherFee = mf1 + mf2 - 1), matcher).isValid(Seq()) shouldBe false
      signed(unsigned.copy(fee = 0), matcher).isValid(Seq()) shouldBe false
      signed(unsigned.copy(fee = -1), matcher).isValid(Seq()) shouldBe false
      signed(unsigned.copy(fee = 4), matcher).isValid(Seq()) shouldBe true
      signed(unsigned.copy(timestamp = maxtTime + 1), matcher).isValid(Seq()) shouldBe false
      signed(unsigned.copy(timestamp = maxtTime), matcher).isValid(Seq()) shouldBe true
      signed(unsigned.copy(timestamp = maxtTime - 1), matcher).isValid(Seq()) shouldBe true
      //TODO check signature and changed orders

    }
  }

  def signed(unsigned: OrderMatch, matcher: PrivateKeyAccount): OrderMatch = {
    val sig = EllipticCurveImpl.sign(matcher, unsigned.toSign)
    OrderMatch(unsigned.order1, unsigned.order2, unsigned.price, unsigned.amount, unsigned.matcherFee, unsigned.fee,
      unsigned.timestamp, sig)

  }

}
