package scorex.lagonaki.integration.api

import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.JsValue
import scorex.lagonaki.TransactionTestingCommons
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl

class PaymentAPISpecification extends FunSuite with Matchers with TransactionTestingCommons {

  import scorex.lagonaki.TestingCommons._

  test("POST /payment API route") {
    //TODO
  }

}