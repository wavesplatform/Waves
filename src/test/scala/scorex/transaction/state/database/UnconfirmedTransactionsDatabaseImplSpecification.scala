package scorex.transaction.state.database

import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers, OneInstancePerTest}
import scorex.account.PublicKeyAccount
import scorex.transaction.{GenesisTransaction, Transaction}

class UnconfirmedTransactionsDatabaseImplSpecification extends FreeSpec
  with Matchers
  with MockFactory
  with OneInstancePerTest {

  private def newTx(id: Long) = GenesisTransaction.create(new PublicKeyAccount(Array.fill(32)(0: Byte)), id, 4598723454L).right.get

  "utx database" - {

    val validator = mockFunction[Transaction, Boolean]

    "do nothing if tx db becomes full" in {

      val db = new UnconfirmedTransactionsDatabaseImpl(1)

      validator expects * returns true once()

      db.putIfNew(newTx(1), validator) shouldBe true

      db.all() should have size 1

      db.putIfNew(newTx(2), validator) shouldBe false
    }

    "does not call validator if same tx comes again" in {

      val db = new UnconfirmedTransactionsDatabaseImpl

      validator expects * returns true once()

      db.putIfNew(newTx(1), validator) shouldBe true
      db.putIfNew(newTx(1), validator) shouldBe false
    }

    "validator returns false" in {

      val db = new UnconfirmedTransactionsDatabaseImpl

      validator expects * returns false

      db.all() shouldBe empty

      db.putIfNew(newTx(1), validator) shouldBe false
      db.all() shouldBe empty
    }
  }
}
