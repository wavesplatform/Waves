package com.wavesplatform.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.account.{AddressScheme, DefaultAddressScheme, PrivateKeyAccount}
import com.wavesplatform.api.http.{ContractInvocationRequest, SignedContractInvocationRequest}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.state._
import com.wavesplatform.transaction.smart.ContractInvocationTransaction.Payment
import com.wavesplatform.transaction.smart.{ContractInvocationTransaction, Verifier}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{JsObject, Json}

class ContractInvocationTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("ContractInvocationTransaction serialization roundtrip") {
    forAll(contractInvokationGen) { transaction: ContractInvocationTransaction =>
      val bytes = transaction.bytes()
      val deser = ContractInvocationTransaction.parseBytes(bytes).get
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
                         "id": "3RRmhhMxbD9SUGUEokaBFmdWqT42vKRiM94tqxuXHE9q",
                         "sender": "3FX9SibfqAWcdnhrmFzqM1mGqya6DkVVnps",
                         "senderPublicKey": "73pu8pHFNpj9tmWuYjqnZ962tXzJvLGX86dxjZxGYhoK",
                         "fee": 100000,
                         "timestamp": 1526910778245,
                         "proofs": ["x7T161SxvUxpubEAKv4UL5ucB5pquAhTryZ8Qrd347TPuQ4yqqpVMQ2B5FpeFXGnpyLvb7wGeoNsyyjh5R61u7F"],
                         "version": 1,
                         "contractAddress" : "3Fb641A9hWy63K18KsBJwns64McmdEATgJd",
                         "call": {
                            "function" : "foo",
                             "args" : [
                             { "key" : "",
                               "type" : "binary",
                               "value" : "base64:YWxpY2U="
                             }
                            ]
                          },
                         "payment" : {
                            "amount" : 7,
                            "assetId" : "73pu8pHFNpj9tmWuYjqnZ962tXzJvLGX86dxjZxGYhoK"
                            }
                        }
    """)

    val tx = ContractInvocationTransaction
      .selfSigned(
        1,
        PrivateKeyAccount("test3".getBytes()),
        PrivateKeyAccount("test4".getBytes()),
        Terms.FUNCTION_CALL(FunctionHeader.User("foo"), List(Terms.CONST_BYTESTR(ByteStr(Base64.decode("YWxpY2U=").get)))),
        Some(ContractInvocationTransaction.Payment(7, Some(ByteStr.decodeBase58("73pu8pHFNpj9tmWuYjqnZ962tXzJvLGX86dxjZxGYhoK").get))),
        100000,
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
    val req = SignedContractInvocationRequest(
      version = 1,
      senderPublicKey = "73pu8pHFNpj9tmWuYjqnZ962tXzJvLGX86dxjZxGYhoK",
      fee = 1,
      call = ContractInvocationRequest.FunctionCallPart("bar", List(BinaryDataEntry("", ByteStr.decodeBase64("YWxpY2U=").get))),
      payment = Some(Payment(1, None)),
      contractAddress = "3Fb641A9hWy63K18KsBJwns64McmdEATgJd",
      timestamp = 11,
      proofs = List("CC1jQ4qkuVfMvB2Kpg2Go6QKXJxUFC8UUswUxBsxwisrR8N5s3Yc8zA6dhjTwfWKfdouSTAnRXCxTXb3T6pJq3T")
    )
    req.toTx shouldBe 'right
    AddressScheme.current = DefaultAddressScheme
  }

}
