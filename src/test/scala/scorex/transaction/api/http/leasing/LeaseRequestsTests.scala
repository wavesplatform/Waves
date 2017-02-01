package scorex.transaction.api.http.leasing

import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.Json
import scorex.api.http.leasing.LeaseRequest

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
}
