package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.account.PrivateKeyAccount
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.assets.TransferTransaction

import scala.util.{Failure, Try}


class TransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  def parseBytes(data: Array[Byte]): Try[TransferTransaction] = {
    data.head match {
      case transactionType: Byte if transactionType == TransactionType.TransferTransaction.id =>
        TransferTransaction.parseTail(data.tail)
      case transactionType =>
        Failure(new Exception(s"Incorrect transaction type '$transactionType' in TransferTransaction data"))
    }
  }

  property("transaction fields should be constructed in a right way") {
    forAll(bytes32gen, bytes32gen, timestampGen, positiveLongGen, positiveLongGen) {
      (senderSeed: Array[Byte], recipientSeed: Array[Byte], time: Long, amount: Long, fee: Long) =>

        val sender = PrivateKeyAccount(senderSeed)
        val recipient = PrivateKeyAccount(recipientSeed)

        val tx = createWavesTransfer(sender, recipient, amount, fee, time).right.get

        tx.timestamp shouldEqual time
        tx.amount shouldEqual amount
        tx.fee shouldEqual fee
        tx.sender shouldEqual sender
        tx.recipient.stringRepr shouldEqual recipient.address
    }
  }

  property("bytes()/parse() roundtrip should preserve a transaction") {
    forAll(bytes32gen, bytes32gen, timestampGen, positiveLongGen, positiveLongGen) {
      (senderSeed: Array[Byte], recipientSeed: Array[Byte], time: Long, amount: Long, fee: Long) =>

        val sender = PrivateKeyAccount(senderSeed)
        val recipient = PrivateKeyAccount(recipientSeed)
        val tx = createWavesTransfer(sender, recipient, amount, fee, time).right.get
        val txAfter = parseBytes(tx.bytes()).get

        txAfter.getClass.shouldBe(tx.getClass)

        tx.signature shouldEqual txAfter.signature
        tx.sender shouldEqual txAfter.asInstanceOf[TransferTransaction].sender
        tx.recipient shouldEqual txAfter.recipient
        tx.timestamp shouldEqual txAfter.timestamp
        tx.amount shouldEqual txAfter.amount
        tx.fee shouldEqual txAfter.fee
    }
  }

  property("TransferTransaction should deserialize to LagonakiTransaction") {
    forAll(bytes32gen, bytes32gen, timestampGen, positiveLongGen, positiveLongGen) {
      (senderSeed: Array[Byte], recipientSeed: Array[Byte], time: Long, amount: Long, fee: Long) =>

        val sender = PrivateKeyAccount(senderSeed)
        val recipient = PrivateKeyAccount(recipientSeed)
        val tx = createWavesTransfer(sender, recipient, amount, fee, time).right.get
        val txAfter = TransactionParser.parseBytes(tx.bytes()).get.asInstanceOf[TransferTransaction]

        txAfter.getClass.shouldBe(tx.getClass)

        tx.signature shouldEqual txAfter.signature
        tx.sender shouldEqual txAfter.asInstanceOf[TransferTransaction].sender
        tx.recipient shouldEqual txAfter.recipient
        tx.timestamp shouldEqual txAfter.timestamp
        tx.amount shouldEqual txAfter.amount
        tx.fee shouldEqual txAfter.fee
    }
  }

}