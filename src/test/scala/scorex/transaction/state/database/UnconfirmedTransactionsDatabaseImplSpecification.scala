package scorex.transaction.state.database

import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers, OneInstancePerTest}
import play.api.libs.json.{JsObject, Json}
import scorex.account.PublicKeyAccount
import scorex.settings.Settings
import scorex.transaction.{GenesisTransaction, Transaction}

class UnconfirmedTransactionsDatabaseImplSpecification extends FreeSpec
  with Matchers
  with MockFactory
  with OneInstancePerTest {

  "utx database" - {

    val validator = mockFunction[Transaction, Boolean]
    val defaultSizedUtxSettings = new Settings {
      override lazy val utxSize = 1000
      override def settingsJSON: JsObject = Json.obj()
    }


    "do nothing if tx db becomes full" in {

      val smallSizedUTXSettings = new Settings {
        override lazy val utxSize = 1
        override def settingsJSON: JsObject = Json.obj()
      }

      val db = new UnconfirmedTransactionsDatabaseImpl(smallSizedUTXSettings)

      validator expects * returns true once()

      db.putIfNew(newTx(1), validator) shouldBe true

      db.all() should have size 1

      db.putIfNew(newTx(2), validator) shouldBe false
    }

    "does not call validator if same tx comes again" in {
      val db = new UnconfirmedTransactionsDatabaseImpl(defaultSizedUtxSettings)

      validator expects * returns true once()

      db.putIfNew(newTx(1), validator) shouldBe true
      db.putIfNew(newTx(1), validator) shouldBe false
    }

    "validator returns false" in {
      val db = new UnconfirmedTransactionsDatabaseImpl(defaultSizedUtxSettings)

      validator expects * returns false

      db.all() shouldBe empty

      db.putIfNew(newTx(1), validator) shouldBe false
      db.all() shouldBe empty
    }
  }

  private def newTx(id: Long) = GenesisTransaction.create(new PublicKeyAccount(Array.fill(32)(0: Byte)), id, 4598723454L).right.get

}
