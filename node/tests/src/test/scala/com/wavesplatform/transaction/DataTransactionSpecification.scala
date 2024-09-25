package com.wavesplatform.transaction

import com.google.common.primitives.Shorts
import com.wavesplatform.account.PublicKey
import com.wavesplatform.api.http.requests.SignedDataRequest
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, DataEntry, EmptyDataEntry, IntegerDataEntry}
import com.wavesplatform.state.DataEntry._
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.serialization.impl.DataTxSerializer
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._
import play.api.libs.json.Json

class DataTransactionSpecification extends PropSpec {

  private def checkSerialization(tx: DataTransaction): Assertion = {
    val parsed = DataTransaction.parseBytes(tx.bytes()).get

    parsed.sender.toAddress.toString shouldEqual tx.sender.toAddress.toString
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

  property("decode pre-encoded bytes") {
    val bytes = Base64.decode(
      "AAwB1SiqvsNcoQDYfHt6EoYy+vGc1EUxgZRXRFEToyoh7yIAAwADaW50AAAAAAAAAAAYAARib29sAQEABGJsb2ICAAVhbGljZQAAAWODBPoKAAAAAAABhqABAAEAQGWOW7SpwumpOCG4fGjUQXv5VRNt1PRVH8+C5J1OyNjxNwJpmm06hc7D143OEcxpzQakHhC5lb09xQ7wtesPa4s="
    )
    val json = Json.parse("""{
                            |  "type": 12,
                            |  "id": "87SfuGJXH1cki2RGDH7WMTGnTXeunkc5mEjNKmmMdRzM",
                            |  "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                            |  "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                            |  "fee": 100000,
                            |  "feeAssetId": null,
                            |  "timestamp": 1526911531530,
                            |  "proofs": [
                            |    "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                            |  ],
                            |  "version": 1,
                            |  "data": [
                            |    {
                            |      "key": "int",
                            |      "type": "integer",
                            |      "value": 24
                            |    },
                            |    {
                            |      "key": "bool",
                            |      "type": "boolean",
                            |      "value": true
                            |    },
                            |    {
                            |      "key": "blob",
                            |      "type": "binary",
                            |      "value": "base64:YWxpY2U="
                            |    }
                            |  ]
                            |}""".stripMargin)

    val tx = DataTxSerializer.parseBytes(bytes).get
    tx.json() shouldBe json
    assert(crypto.verify(tx.signature, tx.bodyBytes(), tx.sender), "signature should be valid")
  }

  property("serialization from TypedTransaction") {
    forAll(dataTransactionGen) { (tx: DataTransaction) =>
      val recovered = DataTransaction.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("unknown type handing") {
    val badTypeIdGen = Gen.choose[Int](DataEntry.Type.maxId + 1, Byte.MaxValue)
    forAll(dataTransactionGen, badTypeIdGen) { case (tx, badTypeId) =>
      val bytes      = tx.bytes()
      val entryCount = Shorts.fromByteArray(bytes.drop(35))
      if (entryCount > 0) {
        val key1Length = Shorts.fromByteArray(bytes.drop(37))
        val p          = 39 + key1Length
        bytes(p) = badTypeId.toByte
        val parsed = DataTransaction.parseBytes(bytes)
        parsed.isFailure shouldBe true
        parsed.failed.get.getMessage shouldBe s"Unknown type $badTypeId"
      }
    }
  }

  property("JSON roundtrip") {
    forAll(dataTransactionGen) { tx =>
      val json = tx.json()
      json.toString shouldEqual tx.toString

      val req = json.as[SignedDataRequest]
      req.senderPublicKey shouldEqual Base58.encode(tx.sender.arr)
      req.fee shouldEqual tx.fee.value
      req.timestamp shouldEqual tx.timestamp

      req.data zip tx.data foreach { case (re, te) =>
        re match {
          case BinaryDataEntry(k, v) =>
            k shouldEqual te.key
            v shouldEqual te.value
          case _: DataEntry[_] =>
            re shouldEqual te
          case _ => fail()
        }
      }
    }
  }

  property("positive validation cases") {
    import DataTransaction.MaxEntryCount
    import com.wavesplatform.state._
    forAll(dataTransactionGen, dataEntryGen(500)) { case (DataTransaction(version, sender, _, fee, timestamp, proofs, chainId), _) =>
      def check(data: List[DataEntry[_]]): Assertion = {
        val txEi = DataTransaction.create(version, sender, data, fee.value, timestamp, proofs)
        txEi shouldBe Right(DataTransaction(version, sender, data, fee, timestamp, proofs, chainId))
        checkSerialization(txEi.explicitGet())
      }

      check(List.empty)                                                               // no data
      check(List.tabulate(MaxEntryCount)(n => IntegerDataEntry(n.toString, n)))       // maximal data
      check(List.tabulate(30)(n => StringDataEntry(n.toString, "a" * 5109)))          // maximal data
      check(List(IntegerDataEntry("a" * MaxKeySize, 0xa)))                            // max key size
      check(List(BinaryDataEntry("bin", ByteStr.empty)))                              // empty binary
      check(List(BinaryDataEntry("bin", ByteStr(Array.fill(MaxValueSize)(1: Byte))))) // max binary value size
      check(List(StringDataEntry("str", "")))                                         // empty string
      check(List(StringDataEntry("str", "A" * MaxValueSize)))                         // max string size
    }
  }

  property("negative validation cases") {
    val gen = Arbitrary.arbBool.arbitrary.flatMap(proto => dataTransactionGen(DataTransaction.MaxEntryCount, withDeleteEntry = proto))
    forAll(gen) { case tx @ DataTransaction(version, sender, data, fee, timestamp, proofs, _) =>
      val emptyKey   = List(IntegerDataEntry("", 2))
      val emptyKeyEi = DataTransaction.create(version, sender, emptyKey, fee.value, timestamp, proofs)
      emptyKeyEi shouldBe Left(TxValidationError.EmptyDataKey)

      val maxKeySize   = MaxPBKeySize
      val keyTooLong   = data :+ BinaryDataEntry("a" * (maxKeySize + 1), ByteStr(Array(1, 2)))
      val keyTooLongEi = DataTransaction.create(version, sender, keyTooLong, fee.value, timestamp, proofs)
      keyTooLongEi shouldBe Left(TxValidationError.TooBigArray)

      val valueTooLong   = data :+ BinaryDataEntry("key", ByteStr(Array.fill(MaxValueSize + 1)(1: Byte)))
      val valueTooLongEi = DataTransaction.create(version, sender, valueTooLong, fee.value, timestamp, proofs)
      valueTooLongEi shouldBe Left(TxValidationError.TooBigArray)

      val e               = BooleanDataEntry("dupe", value = true)
      val duplicateKeys   = e +: data.drop(3) :+ e
      val duplicateKeysEi = DataTransaction.create(version, sender, duplicateKeys, fee.value, timestamp, proofs)
      duplicateKeysEi shouldBe Left(TxValidationError.DuplicatedDataKeys)

      val noFeeEi = DataTransaction.create(1.toByte, sender, data, 0, timestamp, proofs)
      noFeeEi shouldBe Left(TxValidationError.InsufficientFee)

      val negativeFeeEi = DataTransaction.create(1.toByte, sender, data, -100, timestamp, proofs)
      negativeFeeEi shouldBe Left(TxValidationError.InsufficientFee)
    }
  }

  property("JSON format validation") {
    val js = Json.parse("""{
                          |  "type": 12,
                          |  "id": "87SfuGJXH1cki2RGDH7WMTGnTXeunkc5mEjNKmmMdRzM",
                          |  "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                          |  "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                          |  "fee": 100000,
                          |  "feeAssetId": null,
                          |  "timestamp": 1526911531530,
                          |  "proofs": [
                          |    "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                          |  ],
                          |  "version": 1,
                          |  "data": [
                          |    {
                          |      "key": "int",
                          |      "type": "integer",
                          |      "value": 24
                          |    },
                          |    {
                          |      "key": "bool",
                          |      "type": "boolean",
                          |      "value": true
                          |    },
                          |    {
                          |      "key": "blob",
                          |      "type": "binary",
                          |      "value": "base64:YWxpY2U="
                          |    }
                          |  ]
                          |}""".stripMargin)

    val entry1 = IntegerDataEntry("int", 24)
    val entry2 = BooleanDataEntry("bool", value = true)
    val entry3 = BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=")))
    val tx = DataTransaction
      .create(
        1.toByte,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        List(entry1, entry2, entry3),
        100000,
        1526911531530L,
        Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .explicitGet()

    js shouldEqual tx.json()
  }

  property("handle null keys") {
    val emptyDataEntry = EmptyDataEntry("123")

    forAll(accountGen) { sender =>
      val tx1 = DataTransaction.create(TxVersion.V1, sender.publicKey, Seq(emptyDataEntry), 15000000, System.currentTimeMillis(), Proofs.empty)
      tx1 shouldBe Left(GenericError("Empty data is not allowed in V1"))
      val tx2 = DataTransaction.create(TxVersion.V2, sender.publicKey, Seq(emptyDataEntry), 15000000, System.currentTimeMillis(), Proofs.empty)
      tx2.explicitGet()
    }
  }
}
