package scorex.transaction

import com.google.common.primitives.Shorts
import com.wavesplatform.TransactionGen
import com.wavesplatform.state.DataEntry._
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, ByteStr, DataEntry, LongDataEntry}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{Format, Json}
import scorex.api.http.SignedDataRequest
import scorex.crypto.encode.Base58
import scorex.transaction.data.DataTransaction.MaxEntryCount
import scorex.transaction.data.{DataTransaction, DataTransactionParser, DataTransactionV1, DataTransactionV2}

class DataTransactionV1Specification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  private def checkSerialization(tx: DataTransaction): Assertion = {
    val parsed = DataTransactionParser.parseBytes(tx.bytes()).get

    parsed.sender.address shouldEqual tx.sender.address
    parsed.timestamp shouldEqual tx.timestamp
    parsed.fee shouldEqual tx.fee

    parsed.data.zip(tx.data).foreach {
      case (r, t) =>
        r.key shouldEqual t.key
        r.value shouldEqual t.value
    }

    parsed.bytes() shouldEqual tx.bytes()
  }

  property("serialization roundtrip") {
    forAll(dataTransactionGen)(checkSerialization)
  }

  property("serialization from TypedTransaction") {
    forAll(dataTransactionGen) { tx: DataTransaction =>
      val recovered = DataTransactionParser.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("unknown type handing") {
    val badTypeIdGen = Gen.choose[Byte](3, Byte.MaxValue)
    forAll(dataTransactionGen, badTypeIdGen) {
      case (tx: DataTransactionV1, badTypeId) =>
        val bytes      = tx.bytes()
        val entryCount = Shorts.fromByteArray(bytes.drop(35))
        if (entryCount > 0) {
          val key1Length = Shorts.fromByteArray(bytes.drop(37))
          val p          = 39 + key1Length
          bytes(p) = badTypeId
          val parsed = DataTransactionParser.parseBytes(bytes)
          parsed.isFailure shouldBe true
          parsed.failed.get.getMessage shouldBe s"Unknown type $badTypeId"
        }

      case (tx: DataTransactionV2, badTypeId) =>
        val bytes         = tx.bytes()
        val entryCountPos = 35 + tx.recipient.fold(0)(_.bytes.arr.length) + (tx.version - 1)
        val entryCount    = Shorts.fromByteArray(bytes.drop(entryCountPos))
        if (entryCount > 0) {
          val key1Length = Shorts.fromByteArray(bytes.drop(entryCountPos + 2))
          val p          = entryCountPos + 4 + key1Length
          bytes(p) = badTypeId
          val parsed = DataTransactionParser.parseBytes(bytes)
          parsed.isFailure shouldBe true
          parsed.failed.get.getMessage shouldBe s"Unknown type $badTypeId"
        }
    }
  }

  property("JSON roundtrip") {
    implicit val signedFormat: Format[SignedDataRequest] = Json.format[SignedDataRequest]

    forAll(dataTransactionGen) { tx =>
      val json = tx.json()
      json.toString shouldEqual tx.toString

      val req = json.as[SignedDataRequest]
      req.senderPublicKey shouldEqual Base58.encode(tx.sender.publicKey)
      req.fee shouldEqual tx.fee
      req.timestamp shouldEqual tx.timestamp

      req.data zip tx.data foreach {
        case (re, te) =>
          re match {
            case BinaryDataEntry(k, v) =>
              k shouldEqual te.key
              v shouldEqual te.value
            case _: DataEntry[_] =>
              re shouldEqual te
            case _ => fail
          }
      }
    }
  }

  property("positive validation cases") {
    val keyRepeatCountGen = Gen.choose(2, MaxEntryCount)
    forAll(dataTransactionGen, dataEntryGen, keyRepeatCountGen) {
      case (tx, entry, keyRepeatCount) =>
        def check(data: List[DataEntry[_]]): Assertion = {
          val tx1Ei = DataTransactionV1.create(1, tx.sender, data, tx.fee, tx.timestamp, tx.proofs)
          val tx2Ei = DataTransactionV2.create(2, tx.sender, None, data, tx.fee, tx.timestamp, tx.proofs)
          tx1Ei shouldBe Right(DataTransactionV1(1, tx.sender, data, tx.fee, tx.timestamp, tx.proofs))
          tx2Ei shouldBe Right(DataTransactionV2(2, tx.sender, None, data, tx.fee, tx.timestamp, tx.proofs))
          checkSerialization(tx1Ei.right.get)
          checkSerialization(tx2Ei.right.get)
        }

        check(List.empty)                                                      // no data
        check(List.tabulate(MaxEntryCount)(n => LongDataEntry(n.toString, n))) // maximal data
        check(List.fill[DataEntry[_]](keyRepeatCount)(entry))                  // repeating keys
        check(List(BooleanDataEntry("", false)))                               // empty key
        check(List(LongDataEntry("a" * MaxKeySize, 0xa)))                      // max key size
        check(List(BinaryDataEntry("bin", ByteStr.empty)))                     // empty binary
        check(List(BinaryDataEntry("bin", ByteStr(Array.fill(MaxValueSize)(1: Byte))))) // max binary value size
    }
  }

  property("negative validation cases") {
    val badVersionGen = Arbitrary.arbByte.arbitrary.filter(v => !DataTransactionParser.supportedVersions.contains(v))
    forAll(dataTransactionGen, badVersionGen) {
      case (tx, badVersion) =>
        val badVersion1Ei = DataTransactionV1.create(badVersion, tx.sender, tx.data, tx.fee, tx.timestamp, tx.proofs)
        val badVersion2Ei = DataTransactionV2.create(badVersion, tx.sender, tx.recipient, tx.data, tx.fee, tx.timestamp, tx.proofs)
        badVersion1Ei shouldBe Left(ValidationError.UnsupportedVersion(badVersion))
        badVersion2Ei shouldBe Left(ValidationError.UnsupportedVersion(badVersion))

        val dataTooBig    = List.fill(MaxEntryCount + 1)(LongDataEntry("key", 4))
        val dataTooBig1Ei = DataTransactionV1.create(1, tx.sender, dataTooBig, tx.fee, tx.timestamp, tx.proofs)
        val dataTooBig2Ei = DataTransactionV2.create(2, tx.sender, tx.recipient, dataTooBig, tx.fee, tx.timestamp, tx.proofs)
        dataTooBig1Ei shouldBe Left(ValidationError.TooBigArray)
        dataTooBig2Ei shouldBe Left(ValidationError.TooBigArray)

        val keyTooLong    = tx.data :+ BinaryDataEntry("a" * (MaxKeySize + 1), ByteStr(Array(1, 2)))
        val keyTooLong1Ei = DataTransactionV1.create(1, tx.sender, keyTooLong, tx.fee, tx.timestamp, tx.proofs)
        val keyTooLong2Ei = DataTransactionV2.create(2, tx.sender, tx.recipient, keyTooLong, tx.fee, tx.timestamp, tx.proofs)
        keyTooLong1Ei shouldBe Left(ValidationError.TooBigArray)
        keyTooLong2Ei shouldBe Left(ValidationError.TooBigArray)

        val valueTooLong    = tx.data :+ BinaryDataEntry("key", ByteStr(Array.fill(MaxValueSize + 1)(1: Byte)))
        val valueTooLong1Ei = DataTransactionV1.create(1, tx.sender, valueTooLong, tx.fee, tx.timestamp, tx.proofs)
        val valueTooLong2Ei = DataTransactionV2.create(2, tx.sender, tx.recipient, valueTooLong, tx.fee, tx.timestamp, tx.proofs)
        valueTooLong1Ei shouldBe Left(ValidationError.TooBigArray)
        valueTooLong2Ei shouldBe Left(ValidationError.TooBigArray)

        val noFee1Ei = DataTransactionV1.create(1, tx.sender, tx.data, 0, tx.timestamp, tx.proofs)
        val noFee2Ei = DataTransactionV2.create(2, tx.sender, tx.recipient, tx.data, 0, tx.timestamp, tx.proofs)
        noFee1Ei shouldBe Left(ValidationError.InsufficientFee())
        noFee2Ei shouldBe Left(ValidationError.InsufficientFee())

        val negativeFee1Ei = DataTransactionV1.create(1, tx.sender, tx.data, -100, tx.timestamp, tx.proofs)
        val negativeFee2Ei = DataTransactionV2.create(2, tx.sender, tx.recipient, tx.data, -100, tx.timestamp, tx.proofs)
        negativeFee1Ei shouldBe Left(ValidationError.InsufficientFee())
        negativeFee2Ei shouldBe Left(ValidationError.InsufficientFee())
    }
  }
}
