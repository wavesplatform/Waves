package scorex.transaction.api.http.assets

import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.Json
import scorex.api.http.assets.BroadcastRequests.{AssetDeleteRequest, AssetIssueRequest, AssetReissueRequest, AssetTransferRequest}
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

    val tx = req.toTx.get
    tx.name shouldBe "string".toCharArray
    tx.description shouldBe "string".toCharArray
    tx.signature.isEmpty shouldBe false
  }

  test("AssetReissueRequest json parsing works") {
    val json =
      """
        |{
        |"senderPublicKey":"J6JNHaj32DWdgygfPKkiHD7kDFsVM61XuZn44fankgeQ",
        |"assetId":"6eV67ffUPXVGktrmsoWv1ZRKTuKcWZjeCQXJjD26pTGS",
        |"quantity":10000,"reissuable":true,
        |"fee":100000000,"timestamp":1477302582842,
        |"reissuable":true,
        |"signature":"4y7kQ1fwxv61ijgZFrsgSWU6Mxe7A6f4f1jGNXFANxfzK1yVWdKcUMUVZvdZ41JCbqGZKwhmTcfHKV8TYmrmc4QN"
        |}
      """.stripMargin
    val req = Json.parse(json).validate[AssetReissueRequest].get
    req.signature shouldBe "4y7kQ1fwxv61ijgZFrsgSWU6Mxe7A6f4f1jGNXFANxfzK1yVWdKcUMUVZvdZ41JCbqGZKwhmTcfHKV8TYmrmc4QN"
    req.fee shouldBe 100000000L

    val tx = req.toTx.get
    tx.assetId.isEmpty shouldBe false
    tx.reissuable shouldBe true
    tx.signature.isEmpty shouldBe false
  }

  test("AssetTransfer json parsing works") {
    val json =
      """
        |{
        |   "recipient":"3N9UuGeWuDt9NfWbC5oEACHyRoeEMApXAeq",
        |   "timestamp":1479462208828,
        |   "assetId":"GAXAj8T4pSjunDqpz6Q3bit4fJJN9PD4t8AK8JZVSa5u",
        |   "amount":5000,
        |   "fee":100000,
        |   "senderPublicKey":"FJuErRxhV9JaFUwcYLabFK5ENvDRfyJbRz8FeVfYpBLn",
        |   "signature":"4jWTZcRxuFpG4XdCbAhkiWdBjXMHEayPcEhk3yQ3oLYASJ7Fn8ij9C1nAQv61Z7Yo9DoLgy1fysGaaPGbxCWHrfT",
        |   "attachment":""
        |}
      """.stripMargin
    val req = Json.parse(json).validate[AssetTransferRequest].get
    req.recipient shouldBe "3N9UuGeWuDt9NfWbC5oEACHyRoeEMApXAeq"
    req.timestamp shouldBe 1479462208828L
    req.assetId shouldBe Some("GAXAj8T4pSjunDqpz6Q3bit4fJJN9PD4t8AK8JZVSa5u")
    req.amount shouldBe 5000
    req.fee shouldBe 100000
    req.senderPublicKey shouldBe "FJuErRxhV9JaFUwcYLabFK5ENvDRfyJbRz8FeVfYpBLn"
    req.signature shouldBe "4jWTZcRxuFpG4XdCbAhkiWdBjXMHEayPcEhk3yQ3oLYASJ7Fn8ij9C1nAQv61Z7Yo9DoLgy1fysGaaPGbxCWHrfT"
    req.attachment shouldBe Some("")

    val tx = req.toTx.get
    tx.timestamp shouldBe 1479462208828L
    tx.attachment shouldBe Array.emptyByteArray
    tx.assetId.isDefined shouldBe true
    tx.amount shouldBe 5000
    tx.fee shouldBe 100000
    tx.signature.nonEmpty shouldBe true
  }

  test("AssetDeleteRequest json parsing works") {
    val json =
      """
        |{
        |"senderPublicKey":"J6JNHaj32DWdgygfPKkiHD7kDFsVM61XuZn44fankgeQ",
        |"assetId":"6eV67ffUPXVGktrmsoWv1ZRKTuKcWZjeCQXJjD26pTGS",
        |"quantity":10000,
        |"fee":100000000,"timestamp":1477302582842,
        |"signature":"4y7kQ1fwxv61ijgZFrsgSWU6Mxe7A6f4f1jGNXFANxfzK1yVWdKcUMUVZvdZ41JCbqGZKwhmTcfHKV8TYmrmc4QN"
        |}
      """.stripMargin
    val req = Json.parse(json).validate[AssetDeleteRequest].get
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
