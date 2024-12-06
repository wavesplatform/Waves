package com.wavesplatform.transaction

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.transfer.*

class TransactionSpecification extends PropSpec {

  property("transaction fields should be constructed in a right way") {
    forAll(bytes32gen, bytes32gen, timestampGen, positiveLongGen, positiveLongGen) {
      (senderSeed: Array[Byte], recipientSeed: Array[Byte], time: Long, amount: Long, fee: Long) =>
        val sender    = KeyPair(senderSeed)
        val recipient = KeyPair(recipientSeed)

        val tx = createWavesTransfer(sender, recipient.toAddress, amount, fee, time).explicitGet()

        tx.timestamp shouldEqual time
        tx.amount.value shouldEqual amount
        tx.fee.value shouldEqual fee
        tx.sender shouldEqual sender.publicKey
        tx.recipient shouldEqual recipient.toAddress
    }
  }

  property("bytes()/parse() roundtrip should preserve a transaction") {
    forAll(bytes32gen, bytes32gen, timestampGen, positiveLongGen, positiveLongGen) {
      (senderSeed: Array[Byte], recipientSeed: Array[Byte], time: Long, amount: Long, fee: Long) =>
        val sender    = KeyPair(senderSeed)
        val recipient = KeyPair(recipientSeed)
        val tx        = createWavesTransfer(sender, recipient.toAddress, amount, fee, time).explicitGet()
        val txAfter   = TransferTransaction.parseBytes(tx.bytes()).get

        txAfter.getClass.shouldBe(tx.getClass)

        tx.proofs shouldEqual txAfter.proofs
        tx.sender shouldEqual txAfter.sender
        tx.recipient shouldEqual txAfter.recipient
        tx.timestamp shouldEqual txAfter.timestamp
        tx.amount shouldEqual txAfter.amount
        tx.fee shouldEqual txAfter.fee
    }
  }

  property("TransferTransaction should deserialize to LagonakiTransaction") {
    forAll(bytes32gen, bytes32gen, timestampGen, positiveLongGen, positiveLongGen) {
      (senderSeed: Array[Byte], recipientSeed: Array[Byte], time: Long, amount: Long, fee: Long) =>
        val sender    = KeyPair(senderSeed)
        val recipient = KeyPair(recipientSeed)
        val tx        = createWavesTransfer(sender, recipient.toAddress, amount, fee, time).explicitGet()
        val txAfter   = TransactionParsers.parseBytes(tx.bytes()).get.asInstanceOf[TransferTransaction]

        txAfter.getClass.shouldBe(tx.getClass)

        tx.proofs shouldEqual txAfter.proofs
        tx.sender shouldEqual txAfter.sender
        tx.recipient shouldEqual txAfter.recipient
        tx.timestamp shouldEqual txAfter.timestamp
        tx.amount shouldEqual txAfter.amount
        tx.fee shouldEqual txAfter.fee
    }
  }

}
