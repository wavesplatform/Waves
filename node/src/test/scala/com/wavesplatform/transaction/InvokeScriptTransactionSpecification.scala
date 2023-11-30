package com.wavesplatform.transaction

import com.google.protobuf.ByteString
import com.wavesplatform.account.*
import com.wavesplatform.api.http.requests.{InvokeScriptRequest, SignedInvokeScriptRequest}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, *}
import com.wavesplatform.crypto
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{ARR, CONST_BIGINT, CONST_BYTESTR, CONST_LONG, CONST_STRING, CaseObj}
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.serialization.SerdeV1
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader}
import com.wavesplatform.protobuf.transaction.*
import com.wavesplatform.protobuf.{Amount, transaction}
import com.wavesplatform.serialization.Deser
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxHelpers.defaultAddress
import com.wavesplatform.transaction.TxValidationError.NonPositiveAmount
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, Verifier}
import com.wavesplatform.transaction.utils.Signed
import play.api.libs.json.{JsArray, JsObject, JsString, Json}

class InvokeScriptTransactionSpecification extends PropSpec {

  val publicKey = "73pu8pHFNpj9tmWuYjqnZ962tXzJvLGX86dxjZxGYhoK"

  property("InvokeScriptTransaction serialization roundtrip") {
    val transaction = createInvoke()

    val bytes = transaction.bytes()
    val deser = InvokeScriptTransaction.parseBytes(bytes).get
    deser.sender shouldEqual transaction.sender
    deser.dApp shouldEqual transaction.dApp
    deser.funcCallOpt shouldEqual transaction.funcCallOpt
    deser.payments shouldEqual transaction.payments
    deser.fee shouldEqual transaction.fee
    deser.timestamp shouldEqual transaction.timestamp
    deser.proofs shouldEqual transaction.proofs
    bytes shouldEqual deser.bytes()
    Verifier.verifyAsEllipticCurveSignature(transaction, isRideV6Activated = false) should beRight
    Verifier.verifyAsEllipticCurveSignature(deser, isRideV6Activated = false) should beRight // !!!!!!!!!!!!!!!
  }

  property("protobuf roundtrip") {
    val tx     = createInvoke()
    val caller = TxHelpers.defaultSigner

    val unsigned = transaction.PBTransaction(
      tx.chainId,
      ByteString.copyFrom(caller.publicKey.arr),
      Some(Amount.of(PBAmounts.toPBAssetId(tx.feeAssetId), tx.fee.value)),
      tx.timestamp,
      tx.version,
      transaction.PBTransaction.Data.InvokeScript(
        InvokeScriptTransactionData(
          Some(PBRecipients.create(tx.dApp)),
          ByteString.copyFrom(Deser.serializeOption(tx.funcCallOpt)(SerdeV1.serialize(_))),
          tx.payments.map(p => Amount.of(PBAmounts.toPBAssetId(p.assetId), p.amount))
        )
      )
    )
    val proof = crypto.sign(
      caller.privateKey,
      PBTransactions
        .vanilla(PBSignedTransaction(PBSignedTransaction.Transaction.WavesTransaction(unsigned)), unsafe = false)
        .explicitGet()
        .asInstanceOf[ProvenTransaction]
        .bodyBytes()
    )
    val signed       = PBSignedTransaction(PBSignedTransaction.Transaction.WavesTransaction(unsigned), Seq(ByteString.copyFrom(proof.arr)))
    val convTx       = PBTransactions.vanilla(signed, unsafe = false).explicitGet()
    val unsafeConvTx = PBTransactions.vanillaUnsafe(signed)
    val modTx        = tx.copy(sender = caller.publicKey, proofs = Proofs(List(proof)))
    convTx.json() shouldBe modTx.json()
    unsafeConvTx.json() shouldBe modTx.json()
    crypto.verify(modTx.proofs.toSignature, modTx.bodyBytes(), modTx.sender) shouldBe true

    val convToPbTx = PBTransactions.protobuf(modTx)
    convToPbTx shouldBe signed
  }

  property("decode pre-encoded bytes") {
    val bytes = Base64.decode(
      "ABABRFnfcU6tj7ELaOMRU60BmUEXZSyzyWDG4yxX597CilhGAUSJ/UXOr7T3dYRD2dI6xLKS+XNccQNSaToBCQEAAAADZm9vAAAAAQEAAAAFYWxpY2UAAQApAAAAAAAAAAcBWd9xTq2PsQto4xFTrQGZQRdlLLPJYMbjLFfn3sKKWEYAAAAAAAGGoAAAAAFjgvl7hQEAAQBAL4aaBFut6sRjmJqyUMSsW344/xjKn74k0tXmtbAMnZhCIysagYHWE578HZUBuKPxN/3v8OxBmN3lSChpsYrsCg=="
    )
    val json = Json.parse(s"""{
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

    val tx = InvokeScriptTransaction.parseBytes(bytes).get
    tx.json() shouldBe json
    ByteStr(tx.bytes()) shouldBe ByteStr(bytes)
  }

  property("JSON format validation for InvokeScriptTransaction") {
    val dApp = KeyPair("test5".getBytes("UTF-8")).toAddress('D')
    val js = Json.parse(s"""{
                         "type": 16,
                         "id": "6z3CsQBFzV8Wfp1DDiXw5c75LrrwxktPPJTcXYBfTetN",
                         "sender": "3FX9SibfqAWcdnhrmFzqM1mGqya6DkVVnps",
                         "senderPublicKey": "$publicKey",
                         "fee": 100000,
                         "feeAssetId": null,
                         "timestamp": 1526910778245,
                         "proofs": ["x7T161SxvUxpubEAKv4UL5ucB5pquAhTryZ8Qrd347TPuQ4yqqpVMQ2B5FpeFXGnpyLvb7wGeoNsyyjh5R61u7F"],
                         "version": 1,
                         "dApp" : "$dApp",
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

    val tx = Signed.invokeScript(
      1.toByte,
      KeyPair("test3".getBytes("UTF-8")),
      dApp,
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

    (tx.json() - "proofs") shouldEqual (js.asInstanceOf[JsObject] - "proofs")
    TransactionFactory.fromSignedRequest(js) shouldBe Right(tx)
  }

  property("JSON format validation for InvokeScriptTransaction without FUNCTION_CALL") {
    val dApp = KeyPair("test6".getBytes("UTF-8")).toAddress('D')
    val js = Json.parse(s"""{
                         "type": 16,
                         "id": "4sxYQWNDmWvaLwVcmfX1Znj8RfAy7JAWnSQUFgAFFixC",
                         "sender": "3FX9SibfqAWcdnhrmFzqM1mGqya6DkVVnps",
                         "senderPublicKey": "$publicKey",
                         "fee": 100000,
                         "feeAssetId": null,
                         "timestamp": 1526910778245,
                         "proofs": ["3frswEnyFZjTzBQ5pdNEJbPzvLp7Voz8sqZT3n7xsuVDdYGcasXgFNzb8HCrpNXYoDWLsHqrUSqcQfQJ8CRWjp4U"],
                         "version": 1,
                         "dApp" : "$dApp",
                         "payment" : [{
                            "amount" : 7,
                            "assetId" : "$publicKey"
                            }]
                        }
    """)

    val tx = Signed.invokeScript(
      1.toByte,
      KeyPair("test3".getBytes("UTF-8")),
      dApp,
      None,
      Seq(InvokeScriptTransaction.Payment(7, IssuedAsset(ByteStr.decodeBase58(publicKey).get))),
      100000,
      Waves,
      1526910778245L
    )

    (tx.json() - "proofs") shouldEqual (js.asInstanceOf[JsObject] - "proofs" +
      ("call" -> JsObject(Map("function" -> JsString("default"), "args" -> JsArray()))))
    TransactionFactory.fromSignedRequest(js) shouldBe Right(tx)
  }

  property("Signed InvokeScriptTransactionRequest parser") {
    val req = SignedInvokeScriptRequest(
      None,
      Some(1.toByte),
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
      dApp = KeyPair("test7".getBytes("UTF-8")).toAddress('D').toString,
      timestamp = 11,
      proofs =
        Proofs(List("CC1jQ4qkuVfMvB2Kpg2Go6QKXJxUFC8UUswUxBsxwisrR8N5s3Yc8zA6dhjTwfWKfdouSTAnRXCxTXb3T6pJq3T").map(s => ByteStr.decodeBase58(s).get))
    )
    req.toTx.explicitGet()
  }

  property(s"NODE-237, NODE-238. can't have more than ${ContractLimits.MaxInvokeScriptArgs} args") {
    TxHelpers.invoke(defaultAddress, Some(""), Seq.fill(22)(CONST_LONG(0))).funcCallOpt.get.args.length shouldBe 22
    (the[Exception] thrownBy TxHelpers.invoke(defaultAddress, Some(""), Seq.fill(23)(CONST_LONG(0)))).getMessage should include(
      "InvokeScript can't have more than 22 arguments"
    )
  }

  property(s"can call a func with ARR") {
    val pk = PublicKey.fromBase58String(publicKey).explicitGet()
    InvokeScriptTransaction
      .create(
        1.toByte,
        pk,
        pk.toAddress,
        Some(
          Terms.FUNCTION_CALL(
            FunctionHeader.User("foo"),
            List(ARR(IndexedSeq(CONST_LONG(1L), CONST_LONG(2L)), false).explicitGet())
          )
        ),
        Seq(),
        1,
        Waves,
        1,
        Proofs.empty,
        AddressScheme.current.chainId
      )
      .explicitGet()
  }

  property(s"can't call a func with non native(simple) args - CaseObj") {
    val pk = PublicKey.fromBase58String(publicKey).explicitGet()
    InvokeScriptTransaction.create(
      1.toByte,
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
      Proofs.empty,
      AddressScheme.current.chainId
    ) should produce("is unsupported")
  }

  property(s"NODE-97. can't call a func with non native(simple) args - BigInt") {
    val pk = PublicKey.fromBase58String(publicKey).explicitGet()
    InvokeScriptTransaction.create(
      1.toByte,
      pk,
      pk.toAddress,
      Some(
        Terms.FUNCTION_CALL(
          FunctionHeader.User("foo"),
          List(CONST_BIGINT(1))
        )
      ),
      Seq(),
      1,
      Waves,
      1,
      Proofs.empty,
      AddressScheme.current.chainId
    ) should produce("is unsupported")
  }

  property("can't be more 5kb") {
    val largeString = "abcde" * 1024
    val pk          = KeyPair("test8".getBytes("UTF-8")).publicKey
    InvokeScriptTransaction.create(
      1.toByte,
      pk,
      pk.toAddress,
      Some(Terms.FUNCTION_CALL(FunctionHeader.User("foo"), List(Terms.CONST_STRING(largeString).explicitGet()))),
      Seq(),
      1,
      Waves,
      1,
      Proofs.empty,
      AddressScheme.current.chainId
    ) should produce("InvokeScriptTransaction bytes length = 5223 exceeds limit = 5120")
  }

  property("can't have zero amount") {
    val req = SignedInvokeScriptRequest(
      None,
      Some(1.toByte),
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
      dApp = KeyPair("test9".getBytes("UTF-8")).toAddress('D').toString,
      timestamp = 11,
      proofs =
        Proofs(List("CC1jQ4qkuVfMvB2Kpg2Go6QKXJxUFC8UUswUxBsxwisrR8N5s3Yc8zA6dhjTwfWKfdouSTAnRXCxTXb3T6pJq3T").map(s => ByteStr.decodeBase58(s).get))
    )
    req.toTx shouldBe Left(NonPositiveAmount(0, "Waves"))
  }

  property("can't have negative amount") {
    val req = SignedInvokeScriptRequest(
      None,
      Some(1.toByte),
      senderPublicKey = publicKey,
      fee = 1,
      feeAssetId = None,
      call = Some(
        InvokeScriptRequest.FunctionCallPart(
          "bar",
          List(Terms.CONST_BYTESTR(ByteStr.decodeBase64("YWxpY2U=").get).explicitGet())
        )
      ),
      payment = Some(Seq(Payment(-1L, Waves))),
      dApp = KeyPair("test10".getBytes("UTF-8")).toAddress('D').toString,
      timestamp = 11,
      proofs =
        Proofs(List("CC1jQ4qkuVfMvB2Kpg2Go6QKXJxUFC8UUswUxBsxwisrR8N5s3Yc8zA6dhjTwfWKfdouSTAnRXCxTXb3T6pJq3T").map(s => ByteStr.decodeBase58(s).get))
    )
    req.toTx shouldBe Left(NonPositiveAmount(-1, "Waves"))
  }

  private def createInvoke(func: Option[String] = Some("test")): InvokeScriptTransaction =
    TxHelpers.invoke(
      version = TxVersion.V1,
      dApp = TxHelpers.secondAddress,
      func = func,
      args = Seq(CONST_LONG(1), CONST_STRING("test_str").explicitGet(), CONST_BYTESTR(ByteStr(Base64.tryDecode("YWxpY2U=").get)).explicitGet()),
      payments = Seq(Payment(1, Waves), Payment(2, IssuedAsset(ByteStr.decodeBase58(publicKey).get)))
    )
}
