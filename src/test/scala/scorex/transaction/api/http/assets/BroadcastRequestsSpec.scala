package scorex.transaction.api.http.assets

import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.Json
import scorex.api.http.assets.BroadcastRequests.{AssetBurnRequest, AssetIssueRequest, AssetReissueRequest, AssetTransferRequest}
import scorex.api.http.assets.BroadcastResponses.{AssetIssueResponse, AssetReissueResponse, AssetTransferResponse}
import scorex.crypto.encode.Base58


class BroadcastRequestsSpec extends FunSuite with Matchers {

  test("AssetIssueRequest json parsing works") {
    val json =
      """
        {
          "name": "string",
          "quantity": 100000,
          "timestamp": 1484064349669,
          "description": "string",
          "signature": "d3JTwzxHj3j74yemdvw2xAdFeMo6hDJ9i2s3v3e4GMrz2Q25G89Pp3HyXfKk3mwNcKWjtyoTWvTt1eLk2KzetoL",
          "senderPublicKey": "D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5",
          "decimals": 2,
          "reissuable": true,
          "fee": 100000
        }
      """
    val req = Json.parse(json).validate[AssetIssueRequest].get
    req.name shouldBe "string"
    req.quantity shouldBe 100000L
    req.fee shouldBe 100000L
    req.decimals shouldBe 2
    req.timestamp shouldBe 1484064349669L
    req.reissuable shouldBe true

    val tx = req.toTx.right.get
    Base58.encode(tx.name) shouldBe "zVbyBrMk"
    Base58.encode(tx.description) shouldBe "zVbyBrMk"
    tx.reissuable shouldBe true
    tx.decimals shouldBe 2
    tx.fee shouldBe 100000L
    tx.quantity shouldBe 100000L
    tx.timestamp shouldBe 1484064349669L
    Base58.encode(tx.signature) shouldBe "d3JTwzxHj3j74yemdvw2xAdFeMo6hDJ9i2s3v3e4GMrz2Q25G89Pp3HyXfKk3mwNcKWjtyoTWvTt1eLk2KzetoL"
  }

  test("AssetReissueRequest json parsing works") {
    val json =
      """
        |{
        |"senderPublicKey":"D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5",
        |"assetId":"Ha35nwsnmYxHRF8UmKG3S523BycBLZFU4FZnjXryKd4L",
        |"quantity":100000,"reissuable":true,
        |"fee":100000,"timestamp":1234,
        |"reissuable":true,
        |"signature":"4YWbtkDA7PHH1MCxEUaP12pkNRPNqpJh8X7aagZzLyDNbzgopXJb7NHNNV8rjXcy2WsAKX1wzti7Bishu8u6hwtF"
        |}
      """.stripMargin
    val req = Json.parse(json).validate[AssetReissueRequest].get
    req.assetId shouldBe "Ha35nwsnmYxHRF8UmKG3S523BycBLZFU4FZnjXryKd4L"
    req.signature shouldBe "4YWbtkDA7PHH1MCxEUaP12pkNRPNqpJh8X7aagZzLyDNbzgopXJb7NHNNV8rjXcy2WsAKX1wzti7Bishu8u6hwtF"
    req.fee shouldBe 100000L
    req.quantity shouldBe 100000L
    req.timestamp shouldBe 1234L
    req.reissuable shouldBe true

    val tx = req.toTx.right.get
    Base58.encode(tx.assetId) shouldBe "Ha35nwsnmYxHRF8UmKG3S523BycBLZFU4FZnjXryKd4L"
    tx.reissuable shouldBe true
    tx.fee shouldBe 100000L
    tx.quantity shouldBe 100000L
    tx.timestamp shouldBe 1234L
    tx.reissuable shouldBe true
    Base58.encode(tx.signature) shouldBe "4YWbtkDA7PHH1MCxEUaP12pkNRPNqpJh8X7aagZzLyDNbzgopXJb7NHNNV8rjXcy2WsAKX1wzti7Bishu8u6hwtF"
  }

  test("AssetTransfer json parsing works") {
    val json =
      """
        |{
        |   "recipient":"3Myss6gmMckKYtka3cKCM563TBJofnxvfD7",
        |   "timestamp":1479462208828,
        |   "assetId":"GAXAj8T4pSjunDqpz6Q3bit4fJJN9PD4t8AK8JZVSa5u",
        |   "amount":100000,
        |   "fee":100000,
        |   "senderPublicKey":"D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5",
        |   "signature":"4dPRTW6XyRQUTQwwpuZDCNy1UDHYG9WGsEQnn5v49Lj5uyh4XGDdwtEq3t6ZottweAXHieK32UokHwiTxGFtz9bQ",
        |   "attachment":"A"
        |}
      """.stripMargin
    val req = Json.parse(json).validate[AssetTransferRequest].get
    req.recipient shouldBe "3Myss6gmMckKYtka3cKCM563TBJofnxvfD7"
    req.timestamp shouldBe 1479462208828L
    req.assetId shouldBe Some("GAXAj8T4pSjunDqpz6Q3bit4fJJN9PD4t8AK8JZVSa5u")
    req.amount shouldBe 100000
    req.fee shouldBe 100000
    req.senderPublicKey shouldBe "D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5"
    req.signature shouldBe "4dPRTW6XyRQUTQwwpuZDCNy1UDHYG9WGsEQnn5v49Lj5uyh4XGDdwtEq3t6ZottweAXHieK32UokHwiTxGFtz9bQ"
    req.attachment shouldBe Some("A")

    val tx = req.toTx.right.get
    Base58.encode(tx.sender.publicKey) shouldBe "D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5"
    tx.timestamp shouldBe 1479462208828L
    tx.attachment shouldBe Base58.decode("A").get
    Base58.encode(tx.assetId.get) shouldBe "GAXAj8T4pSjunDqpz6Q3bit4fJJN9PD4t8AK8JZVSa5u"
    tx.amount shouldBe 100000
    tx.fee shouldBe 100000
    Base58.encode(tx.signature) shouldBe "4dPRTW6XyRQUTQwwpuZDCNy1UDHYG9WGsEQnn5v49Lj5uyh4XGDdwtEq3t6ZottweAXHieK32UokHwiTxGFtz9bQ"
  }

  test("AssetBurnRequest json parsing works") {
    val json =
      """
        |{
        |"senderPublicKey":"D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5",
        |"assetId":"6eV67ffUPXVGktrmsoWv1ZRKTuKcWZjeCQXJjD26pTGS",
        |"quantity":10000,
        |"fee":100000000,"timestamp":1477302582842,
        |"signature":"H3F8gAsKYeJAPmxCagLaCHycqkr8KiYvzJ4dhophZs31Unmg3dLwVK5k1v1M2Z5zLuQySthpf3DeEyhL6cdpbqp"
        |}
      """.stripMargin
    val req = Json.parse(json).validate[AssetBurnRequest].get
    req.senderPublicKey shouldBe "D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5"
    req.signature shouldBe "H3F8gAsKYeJAPmxCagLaCHycqkr8KiYvzJ4dhophZs31Unmg3dLwVK5k1v1M2Z5zLuQySthpf3DeEyhL6cdpbqp"
    req.fee shouldBe 100000000L
    req.quantity shouldBe 10000
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
