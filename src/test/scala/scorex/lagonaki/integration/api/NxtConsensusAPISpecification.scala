package scorex.lagonaki.integration.api

import org.scalatest.{FunSuite, Matchers}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.lagonaki.TestingCommons

class NxtConsensusAPISpecification extends FunSuite with Matchers {

  import TestingCommons._

  val wallet = application.wallet
  if (wallet.privateKeyAccounts().size < 10) wallet.generateNewAccounts(10)
  val accounts = wallet.privateKeyAccounts()
  val account = accounts.head
  val address = account.address

  test("/consensus/generatingbalance/{address} API route") {
    val response = GET.request(s"/consensus/generatingbalance/$address")
    (response \ "address").as[String] shouldBe address
    (response \ "balance").as[Long] should be >= 0L
  }



}