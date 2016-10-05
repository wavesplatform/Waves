package scorex.transaction

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.crypto.encode.Base58
import scorex.transaction.assets.TransferTransaction

class TransferTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Transfer serialization roundtrip") {
    forAll(transferGenerator) { transfer: TransferTransaction =>
      val recovered = TransferTransaction.parseBytes(transfer.bytes).get

      recovered.sender.address shouldEqual transfer.sender.address
      recovered.assetId.map(_ sameElements transfer.assetId.get).getOrElse(transfer.assetId.isEmpty) shouldBe true
      recovered.feeAsset.map(_ sameElements transfer.feeAsset.get).getOrElse(transfer.feeAsset.isEmpty) shouldBe true
      recovered.timestamp shouldEqual transfer.timestamp
      recovered.amount shouldEqual transfer.amount
      recovered.feeAmount shouldEqual transfer.feeAmount
      recovered.recipient.address shouldEqual transfer.recipient.address

      recovered.bytes shouldEqual transfer.bytes
    }
  }

  property("Transfer serialization from TypedTransaction") {
    forAll(transferGenerator) { tx: TransferTransaction =>
      val recovered = TypedTransaction.parseBytes(tx.bytes).get
      recovered.bytes shouldEqual tx.bytes
    }
  }
  property("TransferTransaction serialization vector2") {
    val amount = 123L
    val timestamp = 1474963563777L
    val feeAmount = 321L
    val bytes = Base58.decode("v6ntRZPdkHXStKR4Fnekm87256j5qyra8HT8fk8F9nSVG3tn64QusK3p2g3Tk5ozQpAxkcf5UuVp8nPHdqCwGrTJ4rM4xeq9XsPqZ8girPFJ4fC6pbYfLWMd5SwkKkKHKvxo5effs395JX6jS2uJqLP1asuQrfaEAuFBmVL6jZUGsqGmTgneAzj1e7TgdWVRh6FVBnmwNhVFusAgkLECbMiyFogEnNtc3wEXF5VW5jDfouGHpjGF6E71BkVJUiLoWP9TfoQ22qC3DQq4rSeoSqq").get

    val recovered = TransferTransaction.parseBytes(bytes).get
    recovered.assetId shouldBe None
    recovered.sender.address shouldBe "3MgucZMc24W9HkdecZGDX3y5nPzfzPZE7ya"
    recovered.recipient.address shouldBe "3MX2WbQQtQjC1oGeAD84WaffNVt3KUtxp1H"
    recovered.amount shouldBe amount
    recovered.timestamp shouldBe timestamp
    Base58.encode(recovered.feeAsset.get) shouldEqual "51TkL91ARXU2kBqck2W2GQ8PUuh18hNqW2MEQ9vtjw3N"
    recovered.feeAmount shouldEqual feeAmount
    Base58.encode(recovered.attachment) shouldBe "2ARHupfAZnhTiikJk3o3mFSGUotf"
    Base58.encode(recovered.signature) shouldBe "4s3BuXSF6Fuxh1mUW9pKr1xx9GxhJ3yNsxqtaAejEW39wFBZMkrfy39aJo2JJDfYwu1x11x4tUa9RQPPA1FyGgc7"
    Base58.encode(recovered.id) shouldEqual "6gfZsDyUepu9aDGPtv1nsy4pzWTebZupNv9iduVrLkdD"

  }

}
