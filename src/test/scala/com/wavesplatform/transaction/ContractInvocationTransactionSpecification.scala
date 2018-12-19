package com.wavesplatform.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.api.http.{ContractInvocationRequest, SignedContractInvocationRequest}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.state._
import com.wavesplatform.transaction.smart.ContractInvocationTransaction
import com.wavesplatform.transaction.smart.ContractInvocationTransaction.Payment
import com.wavesplatform.utils.Base64
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json
import scodec.bits.ByteVector

class ContractInvocationTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("ContractInvocationTransaction serialization roundtrip") {
    forAll(contractInvokationGen) { transaction: ContractInvocationTransaction =>
      val bytes = transaction.bytes()
      val deser = ContractInvocationTransaction.parseBytes(bytes).get
      deser.sender == transaction.sender
      deser.contractAddress == transaction.contractAddress
      deser.fc == transaction.fc
      deser.payment == transaction.payment
      deser.fee == transaction.fee
      deser.timestamp == transaction.timestamp
      deser.proofs == transaction.proofs

    }
  }

  property("JSON format validation for ContractInvocationTransaction") {
    val js = Json.parse("""{
                         "type": 16,
                         "id": "7ZJojVuLsduueo3ktmZS4dxmeJXPWnM4n4aYC11L6TEP",
                         "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                         "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                         "fee": 100000,
                         "timestamp": 1526910778245,
                         "proofs": ["CC1jQ4qkuVfMvB2Kpg2Go6QKXJxUFC8UUswUxBsxwisrR8N5s3Yc8zA6dhjTwfWKfdouSTAnRXCxTXb3T6pJq3T"],
                         "version": 1,
                         "contractAddress" : "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
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
                            "assetId" : "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh"
                            }
                        }
    """)

    val tx = ContractInvocationTransaction
      .create(
        1,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        Terms.FUNCTION_CALL(FunctionHeader.User("foo"), List(Terms.CONST_BYTEVECTOR(ByteVector(Base64.decode("YWxpY2U=").get)))),
        Some(ContractInvocationTransaction.Payment(7, Some(ByteStr.decodeBase58("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh").get))),
        100000,
        1526910778245L,
        Proofs(List(ByteStr.decodeBase58("CC1jQ4qkuVfMvB2Kpg2Go6QKXJxUFC8UUswUxBsxwisrR8N5s3Yc8zA6dhjTwfWKfdouSTAnRXCxTXb3T6pJq3T").get)),
      )
      .right
      .get

    tx.json() shouldEqual js

    println(tx.bytes().map(_.toInt).toList)

    TransactionFactory.fromSignedRequest(js) shouldBe Right(tx)
  }

  property("Signed ContractInvocationTransactionRequest parser") {
    val req = SignedContractInvocationRequest(
      version = 1,
      senderPublicKey = "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
      fee = 1,
      call = ContractInvocationRequest.FunctionCallPart("bar", List(BinaryDataEntry("", ByteStr.decodeBase64("YWxpY2U=").get))),
      payment = Some(Payment(1, None)),
      contractAddress = "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
      timestamp = 11,
      proofs = List("CC1jQ4qkuVfMvB2Kpg2Go6QKXJxUFC8UUswUxBsxwisrR8N5s3Yc8zA6dhjTwfWKfdouSTAnRXCxTXb3T6pJq3T")
    )
    req.toTx shouldBe 'right
  }

}
