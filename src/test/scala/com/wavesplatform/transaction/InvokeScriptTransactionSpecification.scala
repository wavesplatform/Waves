package com.wavesplatform.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.account._
import com.wavesplatform.api.http.{InvokeScriptRequest, SignedInvokeScriptRequest}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, _}
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, Verifier}
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.{JsObject, Json}

class InvokeScriptTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("ContractInvocationTransaction serialization roundtrip") {
    forAll(invokeScriptGen) { transaction: InvokeScriptTransaction =>
      val bytes = transaction.bytes()
      val deser = InvokeScriptTransaction.parseBytes(bytes).get
      deser.sender shouldEqual transaction.sender
      deser.contractAddress shouldEqual transaction.contractAddress
      deser.fc shouldEqual transaction.fc
      deser.payment shouldEqual transaction.payment
      deser.fee shouldEqual transaction.fee
      deser.timestamp shouldEqual transaction.timestamp
      deser.proofs shouldEqual transaction.proofs
      bytes shouldEqual deser.bytes()
      Verifier.verifyAsEllipticCurveSignature(transaction) shouldBe 'right
      Verifier.verifyAsEllipticCurveSignature(deser) shouldBe 'right // !!!!!!!!!!!!!!!
    }
  }

  property("JSON format validation for ContractInvocationTransaction") {
    AddressScheme.current = new AddressScheme { override val chainId: Byte = 'D' }
    val js = Json.parse("""{
                         "type": 16,
                         "id": "AkoaZ4TvxxDcdDXKKfYetpZEaiAVdvdoRmDQYojiARzq",
                         "sender": "3FX9SibfqAWcdnhrmFzqM1mGqya6DkVVnps",
                         "senderPublicKey": "73pu8pHFNpj9tmWuYjqnZ962tXzJvLGX86dxjZxGYhoK",
                         "fee": 100000,
                         "feeAssetId": null,
                         "timestamp": 1526910778245,
                         "proofs": ["x7T161SxvUxpubEAKv4UL5ucB5pquAhTryZ8Qrd347TPuQ4yqqpVMQ2B5FpeFXGnpyLvb7wGeoNsyyjh5R61u7F"],
                         "version": 1,
                         "contractAddress" : "3Fb641A9hWy63K18KsBJwns64McmdEATgJd",
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
                            "assetId" : "73pu8pHFNpj9tmWuYjqnZ962tXzJvLGX86dxjZxGYhoK"
                            }]
                        }
    """)

    val tx = InvokeScriptTransaction
      .selfSigned(
        KeyPair("test3".getBytes()),
        KeyPair("test4".getBytes()),
        Terms.FUNCTION_CALL(FunctionHeader.User("foo"), List(Terms.CONST_BYTESTR(ByteStr(Base64.tryDecode("YWxpY2U=").get)))),
        Seq(InvokeScriptTransaction.Payment(7, IssuedAsset(ByteStr.decodeBase58("73pu8pHFNpj9tmWuYjqnZ962tXzJvLGX86dxjZxGYhoK").get))),
        100000,
        Waves,
        1526910778245L,
      )
      .right
      .get

    (tx.json() - "proofs") shouldEqual (js.asInstanceOf[JsObject] - "proofs")

    TransactionFactory.fromSignedRequest(js) shouldBe Right(tx)
    AddressScheme.current = DefaultAddressScheme
  }

  property("Signed ContractInvocationTransactionRequest parser") {
    AddressScheme.current = new AddressScheme { override val chainId: Byte = 'D' }
    val req = SignedInvokeScriptRequest(
      senderPublicKey = "73pu8pHFNpj9tmWuYjqnZ962tXzJvLGX86dxjZxGYhoK",
      fee = 1,
      feeAssetId = None,
      call = InvokeScriptRequest.FunctionCallPart("bar", List(Terms.CONST_BYTESTR(ByteStr.decodeBase64("YWxpY2U=").get))),
      payment = Some(Seq(Payment(1, Waves))),
      contractAddress = "3Fb641A9hWy63K18KsBJwns64McmdEATgJd",
      timestamp = 11,
      proofs = List("CC1jQ4qkuVfMvB2Kpg2Go6QKXJxUFC8UUswUxBsxwisrR8N5s3Yc8zA6dhjTwfWKfdouSTAnRXCxTXb3T6pJq3T")
    )
    req.toTx shouldBe 'right
    AddressScheme.current = DefaultAddressScheme
  }

  property(s"can't have more than ${ContractLimits.MaxInvokeScriptArgs} args") {
    import com.wavesplatform.common.state.diffs.ProduceError._
    val pk = PublicKey.fromBase58String("73pu8pHFNpj9tmWuYjqnZ962tXzJvLGX86dxjZxGYhoK").explicitGet()
    InvokeScriptTransaction.create(
      pk,
      pk.toAddress,
      Terms.FUNCTION_CALL(FunctionHeader.User("foo"), Range(0, 23).map(_ => Terms.CONST_LONG(0)).toList),
      Seq(),
      1,
      Waves,
      1,
      Proofs.empty
    ) should produce("more than 22 arguments")
  }

  property("can't be more 5kb") {
    val largeString = "abcde" * 1024
    import com.wavesplatform.common.state.diffs.ProduceError._
    val pk = PublicKey.fromBase58String("73pu8pHFNpj9tmWuYjqnZ962tXzJvLGX86dxjZxGYhoK").explicitGet()
    InvokeScriptTransaction.create(
      pk,
      pk.toAddress,
      Terms.FUNCTION_CALL(FunctionHeader.User("foo"), List(Terms.CONST_STRING(largeString))),
      Seq(),
      1,
      Waves,
      1,
      Proofs.empty
    ) should produce("TooBigArray")
  }
}
