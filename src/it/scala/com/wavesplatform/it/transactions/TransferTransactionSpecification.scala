package com.wavesplatform.it.transactions

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, Node}
import com.wavesplatform.it.util._
import com.wavesplatform.state2.ByteStr
import scorex.account.{AccountOrAlias, PrivateKeyAccount}
import scorex.api.http.assets.SignedTransferRequest
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.serialization.BytesSerializable
import scorex.transaction.AssetId

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}

class TransferTransactionSpecification(override val allNodes: Seq[Node]) extends IntegrationSuiteWithThreeAddresses {
  test("asset transfer changes sender's and recipient's asset balance; issuer's waves balance is decreased by fee") {
    val f = for {
      _ <- assertBalances(firstAddress, 100 waves, 100 waves)
      _ <- assertBalances(secondAddress, 100 waves, 100 waves)

      issuedAssetId <- sender.issue(firstAddress, "name", "description", 100000, 2, reissuable = false, fee = 10 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(issuedAssetId))

      _ <- assertBalances(firstAddress, 90 waves, 90 waves)
      _ <- assertAssetBalance(firstAddress, issuedAssetId, 100000)

      transferTransaction <- sender.transfer(firstAddress, secondAddress, 100000, fee = 10 waves, Some(issuedAssetId)).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(transferTransaction))

      _ <- assertBalances(firstAddress, 80 waves, 80 waves)
      _ <- assertBalances(secondAddress, 100 waves, 100 waves)

      _ <- assertAssetBalance(firstAddress, issuedAssetId, 0)
      _ <- assertAssetBalance(secondAddress, issuedAssetId, 100000)
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("waves transfer changes waves balances and eff.b.") {
    val f = for {
      _ <- assertBalances(firstAddress, 80 waves, 80 waves)
      _ <- assertBalances(secondAddress, 100 waves, 100 waves)

      transferId <- sender.transfer(firstAddress, secondAddress, 5 waves, fee = 5 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(transferId))

      _ <- assertBalances(firstAddress, 70 waves, 70 waves)
      _ <- assertBalances(secondAddress, 105 waves, 105 waves)
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("invalid signed waves transfer should not be in UTX or blockchain") {
    def transferToSign(sender: PrivateKeyAccount,
                       recipient: AccountOrAlias,
                       amount: Long,
                       timestamp: Long,
                       feeAmount: Long,
                       attachment: Array[Byte],
                       assetId: Option[AssetId] = None,
                       feeAssetId: Option[AssetId] = None): Array[Byte] = {
      val timestampBytes = Longs.toByteArray(timestamp)
      val assetIdBytes = assetId.map(a => (1: Byte) +: a.arr).getOrElse(Array(0: Byte))
      val amountBytes = Longs.toByteArray(amount)
      val feeAssetIdBytes = feeAssetId.map(a => (1: Byte) +: a.arr).getOrElse(Array(0: Byte))
      val feeBytes = Longs.toByteArray(feeAmount)

      Bytes.concat(Array(4.toByte),
        sender.publicKey,
        assetIdBytes,
        feeAssetIdBytes,
        timestampBytes,
        amountBytes,
        feeBytes,
        recipient.bytes.arr,
        BytesSerializable.arrayWithSize(attachment))
    }

    def createSignedTransferRequest(sender: PrivateKeyAccount,
                                    recipient: AccountOrAlias,
                                    amount: Long,
                                    timestamp: Long,
                                    feeAmount: Long,
                                    attachment: Array[Byte],
                                    assetId: Option[AssetId] = None,
                                    feeAssetId: Option[AssetId] = None): SignedTransferRequest = {
      val signature = ByteStr(EllipticCurveImpl.sign(sender, transferToSign(sender, recipient, amount, timestamp, feeAmount, attachment, assetId, feeAssetId)))

      SignedTransferRequest(
        Base58.encode(sender.publicKey),
        assetId.map(_.base58),
        recipient.stringRepr,
        amount,
        feeAmount,
        feeAssetId.map(_.base58),
        timestamp,
        attachment.headOption.map(_ => Base58.encode(attachment)),
        signature.base58
      )
    }

    val id = ByteStr(FastCryptographicHash(transferToSign(
      PrivateKeyAccount(Base58.decode(sender.accountSeed).get),
      AccountOrAlias.fromString(sender.address).right.get,
      1,
      System.currentTimeMillis() + (1 day).toMillis,
      1 waves,
      Array.emptyByteArray))).base58

    val invalidByTsSignedRequest = createSignedTransferRequest(
      PrivateKeyAccount(Base58.decode(sender.accountSeed).get),
      AccountOrAlias.fromString(sender.address).right.get,
      1,
      System.currentTimeMillis() + (1 day).toMillis,
      1 waves,
      Array.emptyByteArray)

    val f = for {
      _ <- assertBadRequest(sender.signedTransfer(invalidByTsSignedRequest))
      _ <- Future.sequence(allNodes.map(_.isTransactionNotExists(id)))
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("can not make transfer without having enough of fee") {
    val f = for {
      fb <- Future.traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 70 waves, 70 waves)
      _ <- assertBalances(secondAddress, 105 waves, 105 waves)

      transferFailureAssertion <- assertBadRequest(sender.transfer(secondAddress, firstAddress, 104 waves, fee = 2 waves))

      _ <- Future.traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 70 waves, 70 waves)
      _ <- assertBalances(secondAddress, 105 waves, 105 waves)
    } yield transferFailureAssertion

    Await.result(f, 1 minute)
  }


  test("can not make transfer without having enough of waves") {
    val f = for {
      fb <- Future.traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 70 waves, 70 waves)
      _ <- assertBalances(secondAddress, 105 waves, 105 waves)

      transferFailureAssertion <- assertBadRequest(sender.transfer(secondAddress, firstAddress, 106 waves, fee = 1 waves))

      _ <- Future.traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 70 waves, 70 waves)
      _ <- assertBalances(secondAddress, 105 waves, 105 waves)
    } yield transferFailureAssertion

    Await.result(f, 1 minute)
  }

  test("can not make transfer without having enough of effective balance") {
    val f = for {
      fb <- Future.traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 70 waves, 70 waves)
      _ <- assertBalances(secondAddress, 105 waves, 105 waves)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 5 waves, fee = 5 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 65 waves, 60 waves)
      _ <- assertBalances(secondAddress, 105 waves, 110 waves)

      transferFailureAssertion <- assertBadRequest(sender.transfer(firstAddress, secondAddress, 64 waves, fee = 1 waves))

      _ <- Future.traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 65 waves, 60 waves)
      _ <- assertBalances(secondAddress, 105 waves, 110 waves)
    } yield transferFailureAssertion

    Await.result(f, 1 minute)
  }

  test("can not make transfer without having enough of your own waves") {
    val f = for {
      fb <- Future.traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 65 waves, 60 waves)
      _ <- assertBalances(secondAddress, 105 waves, 110 waves)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 5 waves, fee = 5 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 60 waves, 50 waves)
      _ <- assertBalances(secondAddress, 105 waves, 115 waves)

      transferFailureAssertion <- assertBadRequest(sender.transfer(secondAddress, firstAddress, 109 waves, fee = 1 waves))

      _ <- Future.traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 60 waves, 50 waves)
      _ <- assertBalances(secondAddress, 105 waves, 115 waves)
    } yield transferFailureAssertion

    Await.result(f, 1 minute)
  }
}
