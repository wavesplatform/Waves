package scorex.lagonaki.integration.api

import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}
import scorex.api.http.TooBigArrayAllocation
import scorex.crypto.encode.Base58
import scorex.crypto.hash.{FastCryptographicHash, SecureCryptographicHash}

import scala.util.Random

class UtilsAPISpecification extends FunSuite with Matchers with BeforeAndAfterAll {

  import scorex.waves.TestingCommons._

  override protected def beforeAll(): Unit = {
    start()
    stopGeneration(applications)
  }

  override protected def afterAll(): Unit = {
    stop()
  }

  test("/utils/hash/secure API route does't require api key") {
    val msg = "test"
    val resp = POST.request("/utils/hash/secure", body = msg, headers = Map("api_key" -> "wrong key"))
    (resp \ "message").as[String] shouldBe msg
    (resp \ "hash").as[String] shouldBe Base58.encode(SecureCryptographicHash(msg))
  }

  test("/utils/hash/fast API route does't require api key") {
    val msg = "test"
    val resp = POST.request("/utils/hash/fast", body = msg, headers = Map("api_key" -> "wrong key"))
    (resp \ "message").as[String] shouldBe msg
    (resp \ "hash").as[String] shouldBe Base58.encode(FastCryptographicHash(msg))
  }

  test("/utils/seed API route") {
    Base58.decode((GET.request("/utils/seed") \ "seed").as[String]).isSuccess shouldBe true
  }

  test("/utils/seed/{length} API route") {
    val length = Random.nextInt(1024)
    Base58.decode((GET.request(s"/utils/seed/$length") \ "seed").as[String]).get.length shouldBe length
  }

  test("/utils/seed/{length} API route limits argument to 1024") {
    (GET.request(s"/utils/seed/1025") \ "error").as[Int] shouldBe TooBigArrayAllocation.id
  }
}
