package scorex.transaction.api.http.leasing

import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.Json
import scorex.account.{Account, PublicKeyAccount}
import scorex.api.http.leasing.{LeaseCancelRequest, LeaseRequest, SignedLeaseCancelRequest, SignedLeaseRequest}
import scorex.crypto.encode.Base58

class LeaseRequestsTests extends FunSuite with Matchers {

  test("LeaseRequest") {
    val json =
      """
        {
          "amount": 100000,
          "recipient": "3Myss6gmMckKYtka3cKCM563TBJofnxvfD7",
          "sender": "3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb",
          "fee": 1000
        }
      """

    val req = Json.parse(json).validate[LeaseRequest].get

    req shouldBe LeaseRequest("3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb", 100000, 1000, "3Myss6gmMckKYtka3cKCM563TBJofnxvfD7")
  }

  test("LeaseCancelRequest") {
    val json =
      """
        {
          "sender": "3Myss6gmMckKYtka3cKCM563TBJofnxvfD7",
          "txId": "ABMZDPY4MyQz7kKNAevw5P9eNmRErMutJoV9UNeCtqRV",
          "fee": 10000000
        }
      """

    val req = Json.parse(json).validate[LeaseCancelRequest].get

    req shouldBe LeaseCancelRequest("3Myss6gmMckKYtka3cKCM563TBJofnxvfD7", "ABMZDPY4MyQz7kKNAevw5P9eNmRErMutJoV9UNeCtqRV", 10000000)
  }

  test("SignedLeaseRequest") {
    val json =
      """
        {
         "senderPublicKey":"CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw",
         "recipient":"3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb",
         "fee":1000000,
         "timestamp":0,
         "amount":100000,
         "signature":"4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
         }
      """

    val req = Json.parse(json).validate[SignedLeaseRequest].get

    req shouldBe SignedLeaseRequest("CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw",100000L, 1000000L,
      "3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb", 0L, "4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC")
  }

  test("SignedLeaseCancelRequest") {
    val json =
      """
        {
         "senderPublicKey":"CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw",
         "txId":"D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5",
         "timestamp":0,
         "fee": 1000000,
         "signature":"4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
         }
      """

    val req = Json.parse(json).validate[SignedLeaseCancelRequest].get

    req shouldBe SignedLeaseCancelRequest("CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw",
      "D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5", 0L, "4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC", 1000000L)
  }
}
