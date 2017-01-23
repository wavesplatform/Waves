package scorex.lagonaki.integration.api

import org.scalatest.{FunSuite, Matchers}

class NxtConsensusAPISpecification extends FunSuite with Matchers with scorex.waves.TestingCommons {


  override def beforeAll(): Unit = {
    super.beforeAll()
    stopGeneration(applications)
  }

  private val wallet = application.wallet

  if (wallet.privateKeyAccounts().size < 10) wallet.generateNewAccounts(10)

  private val accounts = wallet.privateKeyAccounts()
  private val account = accounts.head
  private val address = account.address

  test("/consensus/generatingbalance/{address} API route") {
    val response = GET.request(s"/consensus/generatingbalance/$address")
    (response \ "address").as[String] shouldBe address
    (response \ "balance").as[Long] should be >= 0L
  }

}
