package scorex.transaction.api.http.leasing

import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.Json
import scorex.api.http.leasing.{LeaseCancelRequest, LeaseRequest, SignedLeaseCancelRequest, SignedLeaseRequest}

class LeaseRequestsTests extends FunSuite with Matchers {

  test("LeaseRequest") {
    val json =
      """
        {
          "amount": 100000,
          "recipient": "3Myss6gmMckKYtka3cKCM563TBJofnxvfD7",
          "description": "string",
          "sender": "D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5",
          "untilBlock": 2,
          "fee": 1000
        }
      """

    val req = Json.parse(json).validate[LeaseRequest].get

    req shouldBe LeaseRequest("D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5", 100000, 1000, 2, "3Myss6gmMckKYtka3cKCM563TBJofnxvfD7")
  }

  test("LeaseCancelRequest") {
    val json =
      """
        {
          "sender": "3Myss6gmMckKYtka3cKCM563TBJofnxvfD7",
          "txId": "D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5"
        }
      """

    val req = Json.parse(json).validate[LeaseCancelRequest].get

    req shouldBe LeaseCancelRequest("3Myss6gmMckKYtka3cKCM563TBJofnxvfD7", "D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5")
  }

  test("SignedLeaseRequest") {
    val json =
      """
        {
         "sender":"3NCUQ2VC4wqtc7gNHt5Biy3BDhYdadxd7cU",
         "recipient":"D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5",
         "fee":1000,
         "timestamp":0,
         "amount":100000,
         "untilBlock":2,
         "signature":"4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
         }
      """

    val req = Json.parse(json).validate[SignedLeaseRequest].get

    req shouldBe SignedLeaseRequest("3NCUQ2VC4wqtc7gNHt5Biy3BDhYdadxd7cU",100000L, 1000L, 2,
      "D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5", 0L, "4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC")
  }

  test("SignedLeaseCancelRequest") {
    val json =
      """
        {
         "sender":"3NCUQ2VC4wqtc7gNHt5Biy3BDhYdadxd7cU",
         "txId":"D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5",
         "timestamp":0,
         "signature":"4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
         }
      """

    val req = Json.parse(json).validate[SignedLeaseCancelRequest].get

    req shouldBe SignedLeaseCancelRequest("3NCUQ2VC4wqtc7gNHt5Biy3BDhYdadxd7cU",
      "D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5", 0L, "4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC")
  }
}
