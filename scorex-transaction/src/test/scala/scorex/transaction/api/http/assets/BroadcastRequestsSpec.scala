package scorex.transaction.api.http.assets

import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.Json
import scorex.api.http.assets.BroadcastRequests.{AssetIssueRequest, AssetReissueRequest}
import scorex.api.http.assets.BroadcastResponses.{AssetIssueResponse, AssetReissueResponse, AssetTransferResponse}


class BroadcastRequestsSpec extends FunSuite with Matchers {

  test("AssetIssueRequest json parsing works") {
    val json =
      """
        {
          "name": "string",
          "quantity": 1000000,
          "timestamp": 1234,
          "description": "string",
          "signature": "string",
          "senderPublicKey": "string",
          "decimals": 2,
          "reissuable": false,
          "fee": 0
        }
      """
    val req = Json.parse(json).validate[AssetIssueRequest].get
    req.name shouldBe "string"
    req.quantity shouldBe 1000000L
    req.fee shouldBe 0L
    req.decimals shouldBe 2
    req.timestamp shouldBe 1234L
    req.reissuable shouldBe false
  }

  test("AssetReissueRequest json parsing works") {
    val json =
      """
        |{
        |"senderPublicKey":"J6JNHaj32DWdgygfPKkiHD7kDFsVM61XuZn44fankgeQ",
        |"assetId":"6eV67ffUPXVGktrmsoWv1ZRKTuKcWZjeCQXJjD26pTGS",
        |"quantity":10000,"reissuable":true,
        |"fee":100000000,"timestamp":1477302582842,
        |"signature":"4y7kQ1fwxv61ijgZFrsgSWU6Mxe7A6f4f1jGNXFANxfzK1yVWdKcUMUVZvdZ41JCbqGZKwhmTcfHKV8TYmrmc4QN"
        |}
      """.stripMargin
    val req = Json.parse(json).validate[AssetReissueRequest].get
    req.signature shouldBe "4y7kQ1fwxv61ijgZFrsgSWU6Mxe7A6f4f1jGNXFANxfzK1yVWdKcUMUVZvdZ41JCbqGZKwhmTcfHKV8TYmrmc4QN"
    req.fee shouldBe 100000000L
  }

  test("AssetIssueResponse json format test") {
    val resp = AssetIssueResponse("id", "assetId", "sndPubKey", "assetName", "desc", 555L, 2.toByte, true, 100L, 132L, "sig")
    val str = Json.toJson(resp).toString
    assert(str == "{\"id\":\"id\",\"assetId\":\"assetId\",\"senderPublicKey\":\"sndPubKey\",\"name\":\"assetName\",\"description\":\"desc\",\"quantity\":555,\"decimals\":2,\"reissuable\":true,\"fee\":100,\"timestamp\":132,\"signature\":\"sig\"}")
  }

  test("AssetReissueResponse json format test") {
    val resp = AssetReissueResponse("id", "assetId", "sndPubKey", 345L, true, 100L, 1L, "sig")
    val str = Json.toJson(resp).toString
    assert(str == "{\"id\":\"id\",\"assetId\":\"assetId\",\"senderPublicKey\":\"sndPubKey\",\"quantity\":345,\"reissuable\":true,\"fee\":100,\"timestamp\":1,\"signature\":\"sig\"}")
  }

  test("AssetTranferResponse json format test") {
    val resp = AssetTransferResponse("id", Some("assetId"), "sndPubKey", "recip", 10L, 1L, 1L, None, "sig")
    val str = Json.toJson(resp).toString
    assert(str == "{\"id\":\"id\",\"assetId\":\"assetId\",\"senderPublicKey\":\"sndPubKey\",\"recipient\":\"recip\",\"amount\":10,\"fee\":1,\"timestamp\":1,\"signature\":\"sig\"}")

    val respA = AssetTransferResponse("id", Some("assetId"), "sndPubKey", "recip", 10L, 1L, 1L, Some("atch"), "sig")
    val strA = Json.toJson(respA).toString
    assert(strA == "{\"id\":\"id\",\"assetId\":\"assetId\",\"senderPublicKey\":\"sndPubKey\",\"recipient\":\"recip\",\"amount\":10,\"fee\":1,\"timestamp\":1,\"attachment\":\"atch\",\"signature\":\"sig\"}")
  }
}
