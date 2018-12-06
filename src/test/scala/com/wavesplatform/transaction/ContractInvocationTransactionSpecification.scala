package com.wavesplatform.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.api.http.SignedContractInvocationRequest
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.state._
import com.wavesplatform.transaction.smart.ContractInvocationTransaction
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
      deser.fee == transaction.fee
      deser.timestamp == transaction.timestamp
      deser.proofs == transaction.proofs

    }
  }

  property("JSON format validation for ContractInvocationTransaction") {
    val js = Json.parse("""{
                         "type": 15,
                         "id": "HVJr2Co9NYVcDrauaV4e2CdViayv5KWBdi5pcZSznw2P",
                         "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                         "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                         "fee": 100000,
                         "timestamp": 1526910778245,
                         "proofs": ["CC1jQ4qkuVfMvB2Kpg2Go6QKXJxUFC8UUswUxBsxwisrR8N5s3Yc8zA6dhjTwfWKfdouSTAnRXCxTXb3T6pJq3T"],
                         "version": 1,
                         "contract" : "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                         "function" : "foo",
                         "args" : [
                           { "key" : "",
                             "type" : "binary",
                             "value" : "base64:YWxpY2U="
                           }
                          ]
                        }
    """)

    val tx = ContractInvocationTransaction
      .create(
        1,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        Terms.FUNCTION_CALL(FunctionHeader.User("foo"), List(Terms.CONST_BYTEVECTOR(ByteVector(Base64.decode("YWxpY2U=").get)))),
        100000,
        1526910778245L,
        Proofs(List(ByteStr.decodeBase58("CC1jQ4qkuVfMvB2Kpg2Go6QKXJxUFC8UUswUxBsxwisrR8N5s3Yc8zA6dhjTwfWKfdouSTAnRXCxTXb3T6pJq3T").get))
      )
      .right
      .get

    js shouldEqual tx.json()
  }

  property("Signed ContractInvocationTransactionRequest parser") {
    val req = SignedContractInvocationRequest(
      version = 1,
      senderPublicKey = "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
      args = List(BinaryDataEntry("", ByteStr.decodeBase64("YWxpY2U=").get)),
      fee = 1,
      function = "bar",
      contractAddress = "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
      timestamp = 11,
      proofs = List("CC1jQ4qkuVfMvB2Kpg2Go6QKXJxUFC8UUswUxBsxwisrR8N5s3Yc8zA6dhjTwfWKfdouSTAnRXCxTXb3T6pJq3T")
    )
    req.toTx shouldBe 'right
  }

}
