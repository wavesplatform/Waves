package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json
import scorex.api.http.SignedDataRequest
import scorex.crypto.encode.Base58
import scorex.transaction.DataTransaction.DataItemSpec
import scorex.transaction.TransactionParser.TransactionType

class DataTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("serialization roundtrip") {
    forAll(dataTransactionGen) { tx: DataTransaction =>
      require(tx.bytes().head == TransactionType.DataTransaction.id)
      val recovered = DataTransaction.parseTail(tx.bytes().tail).get

      recovered.sender.address shouldEqual tx.sender.address
      recovered.timestamp shouldEqual tx.timestamp
      recovered.fee shouldEqual tx.fee

      recovered.data.zip(tx.data).foreach { case (r, t) =>
        r.key shouldEqual t.key
        r.value shouldEqual t.value
      }

      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("serialization from TypedTransaction") {
    forAll(dataTransactionGen) { tx: DataTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON roundtrip") {///needed?
    implicit val specFormat = Json.format[DataItemSpec]
    implicit val signedFormat = Json.format[SignedDataRequest]
//    import DataTransaction.itemFormat
//
//    implicit val dataTransactionFormat: Format[DataTransaction] = Json.format

    forAll(dataTransactionGen) { tx0: DataTransaction =>
      val json0 = tx0.json()
//      Console.err.println("json " + json0)///
      //      val tx1 = json0.as[DataTransaction]
      //      val json1 = tx1.json()
      tx0.toString shouldEqual json0.toString

      val req = json0.as[SignedDataRequest]
//      Console.err.println("req " + req)///
    }
  }

  property("property validation") {
    import DataTransaction._

    forAll(dataTransactionGen) {
      case DataTransaction(version, sender, data, fee, timestamp, proofs) =>
        val dataTooBig = List.fill(MaxDataSize + 1)(IntegerDataItem("key", 4))
        val dataTooBigEi = create(version, sender, dataTooBig, fee, timestamp, proofs)
        dataTooBigEi shouldBe Left(ValidationError.TooBigArray)

        val keyTooLong = data :+ BooleanDataItem("a" * (MaxKeySize + 1), true)
        val keyTooLongEi = create(version, sender, keyTooLong, fee, timestamp, proofs)
        keyTooLongEi shouldBe Left(ValidationError.TooBigArray)

        val valueTooLong = data :+ BinaryDataItem("key", Array.fill(MaxValueSize + 1)(1: Byte))
        val valueTooLongEi = create(version, sender, valueTooLong, fee, timestamp, proofs)
        valueTooLongEi shouldBe Left(ValidationError.TooBigArray)

        val noFeeEi = create(version, sender, data, 0, timestamp, proofs)
        noFeeEi shouldBe Left(ValidationError.InsufficientFee)

        val negativeFeeEi = create(version, sender, data, -100, timestamp, proofs)
        negativeFeeEi shouldBe Left(ValidationError.InsufficientFee)
    }
  }
}
