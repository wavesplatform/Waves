package scorex.transaction

import com.google.common.primitives.Shorts
import com.wavesplatform.TransactionGen
import com.wavesplatform.state2.{BinaryDataEntry, BooleanDataEntry, DataEntry, IntegerDataEntry}
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json
import scorex.api.http.SignedDataRequest
import scorex.crypto.encode.Base58
import scorex.transaction.TransactionParser.TransactionType

class DataTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  private def checkSerialization(tx: DataTransaction): Assertion = {
    require(tx.bytes().head == TransactionType.DataTransaction.id)
    val parsed = DataTransaction.parseTail(tx.bytes().tail).get

    parsed.sender.address shouldEqual tx.sender.address
    parsed.timestamp shouldEqual tx.timestamp
    parsed.fee shouldEqual tx.fee

    parsed.data.zip(tx.data).foreach { case (r, t) =>
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
      val recovered = TransactionParser.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("unknown type handing") {
    val badTypeIdGen = Gen.choose[Byte](3, Byte.MaxValue)
    forAll(dataTransactionGen, badTypeIdGen) { case (tx, badTypeId) =>
      val bytes = tx.bytes()
      val entryCount = Shorts.fromByteArray(bytes.drop(34))
      if (entryCount > 0) {
        val p = 36 + bytes(36) // bytes(36) is key#1 length
        val parsed = DataTransaction.parseTail((bytes.tail.take(p) :+ badTypeId) ++ bytes.drop(p + 2))
        parsed.isFailure shouldBe true
        parsed.failed.get.getMessage shouldBe s"Unknown type $badTypeId"
      }
    }
  }

  property("JSON roundtrip") {///needed?
//    implicit val specFormat = Json.format[DataItemSpec]
    implicit val signedFormat = Json.format[SignedDataRequest]
//    import DataTransaction.itemFormat
//
//    implicit val dataTransactionFormat: Format[DataTransaction] = Json.format

    forAll(dataTransactionGen) { tx0: DataTransaction =>
      val json0 = tx0.json()
      Console.err.println("json " + json0)///
//      val tx1 = json0.as[DataTransaction]
//      val json1 = tx1.json()
      tx0.toString shouldEqual json0.toString

      val req = json0.as[SignedDataRequest]
      req.senderPublicKey shouldEqual Base58.encode(tx0.sender.publicKey)
      req.fee shouldEqual tx0.fee
      req.timestamp shouldEqual tx0.timestamp

      val reqData = req.data//.map(_.parse.right.get)
      reqData(0) shouldEqual tx0.data(0)
      reqData(1) shouldEqual tx0.data(1)
      reqData.zip(tx0.data).apply(2) match {
        case (BinaryDataEntry(rk, rv), BinaryDataEntry(tk, tv)) =>
          rk shouldEqual tk
          rv shouldEqual tv
        case _ => fail
      }
    }
  }

  property("positive validation cases") {
    import DataTransaction._
    import com.wavesplatform.state2.DataEntry._

    val keyRepeatCountGen = Gen.choose(2, MaxEntryCount)
    forAll(dataTransactionGen, keyRepeatCountGen) {
      case (DataTransaction(version, sender, data, fee, timestamp, proofs), keyRepeatCount) =>
        def check(data: List[DataEntry[_]]): Assertion = {
          val txEi = create(version, sender, data, fee, timestamp, proofs)
          txEi shouldBe Right(DataTransaction(version, sender, data, fee, timestamp, proofs))
          checkSerialization(txEi.right.get)
        }

        check(List.empty)  // no data
        check(List.tabulate(MaxEntryCount)(n => IntegerDataEntry(n.toString, n)))  // maximal data
        check(List.fill[DataEntry[_]](keyRepeatCount)(data.head))  // repeating keys
        check(List(BooleanDataEntry("", false)))  // empty key
        check(List(IntegerDataEntry("a" * Byte.MaxValue, 0xa)))  // max key size
        check(List(BinaryDataEntry("bin", Array.empty)))  // empty binary
        check(List(BinaryDataEntry("bin", Array.fill(MaxValueSize)(1: Byte))))  // max binary value size
    }
  }

  property("negative validation cases") {
    import DataTransaction._
    import com.wavesplatform.state2.DataEntry._

    val badVersionGen = Gen.choose(Proofs.Version + 1, Byte.MaxValue).map(_.toByte)
    forAll(dataTransactionGen, badVersionGen) {
      case (DataTransaction(version, sender, data, fee, timestamp, proofs), badVersion) =>
        val badVersionEi = create(badVersion, sender, data, fee, timestamp, proofs)
        badVersionEi shouldBe Left(ValidationError.UnsupportedVersion(badVersion))

        val dataTooBig = List.fill(MaxEntryCount + 1)(IntegerDataEntry("key", 4))
        val dataTooBigEi = create(version, sender, dataTooBig, fee, timestamp, proofs)
        dataTooBigEi shouldBe Left(ValidationError.TooBigArray)

        val keyTooLong = data :+ BinaryDataEntry("a" * (MaxKeySize + 1), Array(1, 2))
        val keyTooLongEi = create(version, sender, keyTooLong, fee, timestamp, proofs)
        keyTooLongEi shouldBe Left(ValidationError.TooBigArray)

        val valueTooLong = data :+ BinaryDataEntry("key", Array.fill(MaxValueSize + 1)(1: Byte))
        val valueTooLongEi = create(version, sender, valueTooLong, fee, timestamp, proofs)
        valueTooLongEi shouldBe Left(ValidationError.TooBigArray)

        val noFeeEi = create(version, sender, data, 0, timestamp, proofs)
        noFeeEi shouldBe Left(ValidationError.InsufficientFee)

        val negativeFeeEi = create(version, sender, data, -100, timestamp, proofs)
        negativeFeeEi shouldBe Left(ValidationError.InsufficientFee)
    }
  }
}
