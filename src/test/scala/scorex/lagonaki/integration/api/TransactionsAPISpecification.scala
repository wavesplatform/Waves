package scorex.lagonaki.integration.api

import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.JsValue
import scorex.lagonaki.TransactionTestingCommons
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl

class TransactionsAPISpecification extends FunSuite with Matchers with TransactionTestingCommons {

  import scorex.lagonaki.TestingCommons._

  if (wallet.privateKeyAccounts().size < 10) wallet.generateNewAccounts(10)
  val addresses = accounts.map(_.address)
  val account = accounts.head
  val address = account.address

  test("/transactions/unconfirmed API route") {
    (1 to 20) foreach (i => genValidTransaction())
    val unconfirmed = UnconfirmedTransactionsDatabaseImpl.all()
    unconfirmed.size should be > 0
    val tr = GET.request("/transactions/unconfirmed")
    (tr \\ "signature").toList.size shouldBe unconfirmed.size
  }

  test("/transactions/address/{address} API route") {
    addresses.foreach { a =>
      checkTransactionList(GET.request(s"/transactions/address/$a"))
    }
  }

  test("/transactions/address/{address}/limit/{limit} API route") {
    addresses.foreach { a =>
      val tr = GET.request(s"/transactions/address/$a/limit/2")
      (tr \\ "amount").toList.size should be <= 2
      checkTransactionList(tr)
    }
  }

  def checkTransactionList(tr: JsValue): Unit = {
    (tr \\ "amount").toList.foreach(amount => amount.as[String].replace("\"", "").toLong should be > 0L)
    (tr \\ "fee").toList.foreach(amount => amount.as[Long] should be >= 0L)
    (tr \\ "type").toList.foreach(amount => amount.as[Int] should be >= 0)
    (tr \\ "timestamp").toList.foreach(amount => amount.as[Long] should be >= 0L)
    (tr \\ "signature").toList.size should be >= 0
    (tr \\ "sender").toList.size should be >= 0
    (tr \\ "recipient").toList.size should be >= 0
  }


}