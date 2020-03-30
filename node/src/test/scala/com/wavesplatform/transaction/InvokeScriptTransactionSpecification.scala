package com.wavesplatform.transaction

import com.google.protobuf.ByteString
import com.wavesplatform.account._
import com.wavesplatform.api.http.{InvokeScriptRequest, SignedInvokeScriptRequest}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, _}
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{ARR, CONST_LONG, CaseObj}
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader, Serde}
import com.wavesplatform.protobuf.transaction._
import com.wavesplatform.protobuf.{Amount, transaction}
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.NonPositiveAmount
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, Verifier}
import com.wavesplatform.{TransactionGen, crypto}
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.{JsObject, Json}

class InvokeScriptTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  val publicKey = "73pu8pHFNpj9tmWuYjqnZ962tXzJvLGX86dxjZxGYhoK"

  property("InvokeScriptTransaction serialization roundtrip") {
    forAll(invokeScriptGen) { transaction: InvokeScriptTransaction =>
      val bytes = transaction.bytes()
      val deser = InvokeScriptTransaction.parseBytes(bytes).get
      deser.sender shouldEqual transaction.sender
      deser.dAppAddressOrAlias shouldEqual transaction.dAppAddressOrAlias
      deser.funcCallOpt shouldEqual transaction.funcCallOpt
      deser.payment shouldEqual transaction.payment
      deser.fee shouldEqual transaction.fee
      deser.timestamp shouldEqual transaction.timestamp
      deser.proofs shouldEqual transaction.proofs
      bytes shouldEqual deser.bytes()
      Verifier.verifyAsEllipticCurveSignature(transaction) shouldBe 'right
      Verifier.verifyAsEllipticCurveSignature(deser) shouldBe 'right // !!!!!!!!!!!!!!!
    }
  }

  property("protobuf roundtrip") {
    forAll(invokeScriptGen, accountGen) { (tx, caller) =>
      val unsigned = transaction.PBTransaction(
        tx.chainId,
        ByteString.copyFrom(caller.publicKey),
        Some(Amount.of(PBAmounts.toPBAssetId(tx.feeAssetId), tx.fee)),
        tx.timestamp,
        tx.version,
        transaction.PBTransaction.Data.InvokeScript(
          InvokeScriptTransactionData(
            Some(PBRecipients.create(tx.dAppAddressOrAlias)),
            ByteString.copyFrom(Deser.serializeOptionOfArray(tx.funcCallOpt)(Serde.serialize(_))),
            tx.payment.map(p => Amount.of(PBAmounts.toPBAssetId(p.assetId), p.amount))
          )
        )
      )
      val proof  = crypto.sign(caller, PBTransactions.vanilla(SignedTransaction(Some(unsigned))).explicitGet().bodyBytes())
      val signed = SignedTransaction(Some(unsigned), Seq(ByteString.copyFrom(proof)))
      val convTx = PBTransactions.vanilla(signed).explicitGet()
      val convUnsafeTx = PBTransactions.vanilla(signed, unsafe = true).explicitGet()
      val modTx  = tx.copy(sender = caller.publicKey, proofs = Proofs(List(proof)))
      convTx.json() shouldBe modTx.json()
      convUnsafeTx.json() shouldBe modTx.json()
      crypto.verify(modTx.proofs.toSignature, modTx.bodyBytes(), modTx.sender) shouldBe true

      val convToPbTx = PBTransactions.protobuf(modTx)
      convToPbTx shouldBe signed
    }
  }

  property("JSON format validation for InvokeScriptTransaction") {
    AddressScheme.current = new AddressScheme { override val chainId: Byte = 'D' }
    val js = Json.parse(s"""{
                         "type": 16,
                         "id": "F4Kf5GZqAEnfTgaK9Zj9CypXApE6M4yYGR2DQ3yMhjwF",
                         "sender": "3FX9SibfqAWcdnhrmFzqM1mGqya6DkVVnps",
                         "senderPublicKey": "$publicKey",
                         "fee": 100000,
                         "feeAssetId": null,
                         "timestamp": 1526910778245,
                         "proofs": ["x7T161SxvUxpubEAKv4UL5ucB5pquAhTryZ8Qrd347TPuQ4yqqpVMQ2B5FpeFXGnpyLvb7wGeoNsyyjh5R61u7F"],
                         "version": 1,
                         "dApp" : "3Fb641A9hWy63K18KsBJwns64McmdEATgJd",
                         "call": {
                            "function" : "foo",
                             "args" : [
                             { "type" : "binary",
                               "value" : "base64:YWxpY2U="
                             }
                            ]
                          },
                         "payment" : [{
                            "amount" : 7,
                            "assetId" : "$publicKey"
                            }]
                        }
    """)

    val tx = InvokeScriptTransaction
      .selfSigned(
        KeyPair("test3".getBytes("UTF-8")),
        KeyPair("test4".getBytes("UTF-8")),
        Some(
          Terms.FUNCTION_CALL(
            FunctionHeader.User("foo"),
            List(Terms.CONST_BYTESTR(ByteStr(Base64.tryDecode("YWxpY2U=").get)).explicitGet())
          )
        ),
        Seq(InvokeScriptTransaction.Payment(7, IssuedAsset(ByteStr.decodeBase58(publicKey).get))),
        100000,
        Waves,
        1526910778245L
      )
      .right
      .get

    (tx.json() - "proofs") shouldEqual (js.asInstanceOf[JsObject] - "proofs")

    TransactionFactory.fromSignedRequest(js) shouldBe Right(tx)
    AddressScheme.current = DefaultAddressScheme
  }

  property("JSON format validation for InvokeScriptTransaction without FUNCTION_CALL") {
    AddressScheme.current = new AddressScheme { override val chainId: Byte = 'D' }
    val js = Json.parse(s"""{
                         "type": 16,
                         "id": "CoAdpSKQWNJEieS9StynH8VwmaSPbKSFKEYWinkTGHBF",
                         "sender": "3FX9SibfqAWcdnhrmFzqM1mGqya6DkVVnps",
                         "senderPublicKey": "$publicKey",
                         "fee": 100000,
                         "feeAssetId": null,
                         "timestamp": 1526910778245,
                         "proofs": ["3frswEnyFZjTzBQ5pdNEJbPzvLp7Voz8sqZT3n7xsuVDdYGcasXgFNzb8HCrpNXYoDWLsHqrUSqcQfQJ8CRWjp4U"],
                         "version": 1,
                         "dApp" : "3Fb641A9hWy63K18KsBJwns64McmdEATgJd",
                         "payment" : [{
                            "amount" : 7,
                            "assetId" : "$publicKey"
                            }]
                        }
    """)

    val tx = InvokeScriptTransaction
      .selfSigned(
        KeyPair("test3".getBytes("UTF-8")),
        KeyPair("test4".getBytes("UTF-8")),
        None,
        Seq(InvokeScriptTransaction.Payment(7, IssuedAsset(ByteStr.decodeBase58(publicKey).get))),
        100000,
        Waves,
        1526910778245L
      )
      .right
      .get

    (tx.json() - "proofs") shouldEqual (js.asInstanceOf[JsObject] - "proofs")

    TransactionFactory.fromSignedRequest(js) shouldBe Right(tx)
    AddressScheme.current = DefaultAddressScheme
  }

  property("Signed InvokeScriptTransactionRequest parser") {
    AddressScheme.current = new AddressScheme { override val chainId: Byte = 'D' }
    val req = SignedInvokeScriptRequest(
      senderPublicKey = publicKey,
      fee = 1,
      feeAssetId = None,
      call = Some(
        InvokeScriptRequest.FunctionCallPart(
          "bar",
          List(Terms.CONST_BYTESTR(ByteStr.decodeBase64("YWxpY2U=").get).explicitGet())
        )
      ),
      payment = Some(Seq(Payment(1, Waves))),
      dApp = "3Fb641A9hWy63K18KsBJwns64McmdEATgJd",
      timestamp = 11,
      proofs = List("CC1jQ4qkuVfMvB2Kpg2Go6QKXJxUFC8UUswUxBsxwisrR8N5s3Yc8zA6dhjTwfWKfdouSTAnRXCxTXb3T6pJq3T")
    )
    req.toTx shouldBe 'right
    AddressScheme.current = DefaultAddressScheme
  }

  property(s"can't have more than ${ContractLimits.MaxInvokeScriptArgs} args") {
    import com.wavesplatform.common.state.diffs.ProduceError._
    val pk = PublicKey.fromBase58String(publicKey).explicitGet()
    InvokeScriptTransaction.create(
      pk,
      pk.toAddress,
      Some(Terms.FUNCTION_CALL(FunctionHeader.User("foo"), Range(0, 23).map(_ => Terms.CONST_LONG(0)).toList)),
      Seq(),
      1,
      Waves,
      1,
      Proofs.empty
    ) should produce("more than 22 arguments")
  }

  property(s"can't call a func with non native(simple) args - ARR") {
    import com.wavesplatform.common.state.diffs.ProduceError._
    val pk = PublicKey.fromBase58String(publicKey).explicitGet()
    InvokeScriptTransaction.create(
      pk,
      pk.toAddress,
      Some(
        Terms.FUNCTION_CALL(
          FunctionHeader.User("foo"),
          List(ARR(IndexedSeq(CONST_LONG(1L), CONST_LONG(2L))).explicitGet())
        )
      ),
      Seq(),
      1,
      Waves,
      1,
      Proofs.empty
    ) should produce("All arguments of invokeScript must be one of the types")
  }

  property(s"can't call a func with non native(simple) args - CaseObj") {
    import com.wavesplatform.common.state.diffs.ProduceError._
    val pk = PublicKey.fromBase58String(publicKey).explicitGet()
    InvokeScriptTransaction.create(
      pk,
      pk.toAddress,
      Some(
        Terms.FUNCTION_CALL(
          FunctionHeader.User("foo"),
          List(CaseObj(CASETYPEREF("SHA256", List.empty), Map("tmpKey" -> CONST_LONG(42))))
        )
      ),
      Seq(),
      1,
      Waves,
      1,
      Proofs.empty
    ) should produce("All arguments of invokeScript must be one of the types")
  }

  property("can't be more 5kb") {
    val largeString = "abcde" * 1024
    import com.wavesplatform.common.state.diffs.ProduceError._
    val pk = PublicKey.fromBase58String(publicKey).explicitGet()
    InvokeScriptTransaction.create(
      pk,
      pk.toAddress,
      Some(Terms.FUNCTION_CALL(FunctionHeader.User("foo"), List(Terms.CONST_STRING(largeString).explicitGet()))),
      Seq(),
      1,
      Waves,
      1,
      Proofs.empty
    ) should produce("TooBigArray")
  }

  property("can't have zero amount") {
    AddressScheme.current = new AddressScheme { override val chainId: Byte = 'D' }
    val req = SignedInvokeScriptRequest(
      senderPublicKey = publicKey,
      fee = 1,
      feeAssetId = None,
      call = Some(
        InvokeScriptRequest.FunctionCallPart(
          "bar",
          List(Terms.CONST_BYTESTR(ByteStr.decodeBase64("YWxpY2U=").get).explicitGet())
        )
      ),
      payment = Some(Seq(Payment(0, Waves))),
      dApp = "3Fb641A9hWy63K18KsBJwns64McmdEATgJd",
      timestamp = 11,
      proofs = List("CC1jQ4qkuVfMvB2Kpg2Go6QKXJxUFC8UUswUxBsxwisrR8N5s3Yc8zA6dhjTwfWKfdouSTAnRXCxTXb3T6pJq3T")
    )
    req.toTx shouldBe Left(NonPositiveAmount(0, "Waves"))
    AddressScheme.current = DefaultAddressScheme
  }

  property("can't have negative amount") {
    AddressScheme.current = new AddressScheme { override val chainId: Byte = 'D' }
    val req = SignedInvokeScriptRequest(
      senderPublicKey = publicKey,
      fee = 1,
      feeAssetId = None,
      call = Some(
        InvokeScriptRequest.FunctionCallPart(
          "bar",
          List(Terms.CONST_BYTESTR(ByteStr.decodeBase64("YWxpY2U=").get).explicitGet())
        )
      ),
      payment = Some(Seq(Payment(-1, Waves))),
      dApp = "3Fb641A9hWy63K18KsBJwns64McmdEATgJd",
      timestamp = 11,
      proofs = List("CC1jQ4qkuVfMvB2Kpg2Go6QKXJxUFC8UUswUxBsxwisrR8N5s3Yc8zA6dhjTwfWKfdouSTAnRXCxTXb3T6pJq3T")
    )
    req.toTx shouldBe Left(NonPositiveAmount(-1, "Waves"))
    AddressScheme.current = DefaultAddressScheme
  }
}
