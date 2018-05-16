package scorex.transaction

import com.google.common.primitives.Shorts
import com.wavesplatform.TransactionGen
import com.wavesplatform.state.DataEntry._
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, ByteStr, DataEntry, LongDataEntry, StringDataEntry}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{Format, Json}
import scorex.api.http.SignedDataRequest
import scorex.crypto.encode.Base58
import scorex.transaction.DataTransaction.MaxEntryCount

class DataTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  private def checkSerialization(tx: DataTransaction): Assertion = {
    val parsed = DataTransaction.parseBytes(tx.bytes()).get

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
      val recovered = DataTransaction.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("unknown type handing") {
    val badTypeIdGen = Gen.choose[Byte](3, Byte.MaxValue)
    forAll(dataTransactionGen, badTypeIdGen) {
      case (tx, badTypeId) =>
        val bytes      = tx.bytes()
        val entryCount = Shorts.fromByteArray(bytes.drop(35))
        if (entryCount > 0) {
          val key1Length = Shorts.fromByteArray(bytes.drop(37))
          val p          = 39 + key1Length
          bytes(p) = badTypeId
          val parsed = DataTransaction.parseBytes(bytes)
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
    import com.wavesplatform.state._
    val keyRepeatCountGen = Gen.choose(2, MaxEntryCount)
    forAll(dataTransactionGen, dataEntryGen, keyRepeatCountGen) {
      case (DataTransaction(version, sender, data, fee, timestamp, proofs), entry, keyRepeatCount) =>
        def check(data: List[DataEntry[_]]): Assertion = {
          val txEi = DataTransaction.create(version, sender, data, fee, timestamp, proofs)
          txEi shouldBe Right(DataTransaction(version, sender, data, fee, timestamp, proofs))
          checkSerialization(txEi.explicitGet())
        }

        check(List.empty)                                                               // no data
        check(List.tabulate(MaxEntryCount)(n => LongDataEntry(n.toString, n)))          // maximal data
        check(List.fill[DataEntry[_]](keyRepeatCount)(entry))                           // repeating keys
        check(List(BooleanDataEntry("", false)))                                        // empty key
        check(List(LongDataEntry("a" * MaxKeySize, 0xa)))                               // max key size
        check(List(BinaryDataEntry("bin", ByteStr.empty)))                              // empty binary
        check(List(BinaryDataEntry("bin", ByteStr(Array.fill(MaxValueSize)(1: Byte))))) // max binary value size
        check(List(StringDataEntry("str", "")))                                         // empty string
        check(List(StringDataEntry("str", "A" * MaxValueSize))) // max string size
    }
  }

  property("negative validation cases") {
    val badVersionGen = Arbitrary.arbByte.arbitrary.filter(v => !DataTransaction.supportedVersions.contains(v))
    forAll(dataTransactionGen, badVersionGen) {
      case (DataTransaction(version, sender, data, fee, timestamp, proofs), badVersion) =>
        val badVersionEi = DataTransaction.create(badVersion, sender, data, fee, timestamp, proofs)
        badVersionEi shouldBe Left(ValidationError.UnsupportedVersion(badVersion))

        val dataTooBig   = List.fill(MaxEntryCount + 1)(LongDataEntry("key", 4))
        val dataTooBigEi = DataTransaction.create(version, sender, dataTooBig, fee, timestamp, proofs)
        dataTooBigEi shouldBe Left(ValidationError.TooBigArray)

        val keyTooLong   = data :+ BinaryDataEntry("a" * (MaxKeySize + 1), ByteStr(Array(1, 2)))
        val keyTooLongEi = DataTransaction.create(version, sender, keyTooLong, fee, timestamp, proofs)
        keyTooLongEi shouldBe Left(ValidationError.TooBigArray)

        val valueTooLong   = data :+ BinaryDataEntry("key", ByteStr(Array.fill(MaxValueSize + 1)(1: Byte)))
        val valueTooLongEi = DataTransaction.create(version, sender, valueTooLong, fee, timestamp, proofs)
        valueTooLongEi shouldBe Left(ValidationError.TooBigArray)

        val noFeeEi = DataTransaction.create(version, sender, data, 0, timestamp, proofs)
        noFeeEi shouldBe Left(ValidationError.InsufficientFee())

        val negativeFeeEi = DataTransaction.create(version, sender, data, -100, timestamp, proofs)
        negativeFeeEi shouldBe Left(ValidationError.InsufficientFee())
    }
  }
}
