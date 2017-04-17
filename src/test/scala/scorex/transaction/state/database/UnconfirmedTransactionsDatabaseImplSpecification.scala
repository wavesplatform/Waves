package scorex.transaction.state.database

import com.wavesplatform.settings.UTXSettings
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers, OneInstancePerTest}
import scorex.account.PublicKeyAccount
import scorex.transaction.{GenesisTransaction, Transaction}

import scala.concurrent.duration._

class UnconfirmedTransactionsDatabaseImplSpecification extends FreeSpec
  with Matchers
  with MockFactory
  with OneInstancePerTest {

  "utx database" - {

    val validator = mockFunction[Transaction, Boolean]
    val defaultSizedUtxSettings = UTXSettings(1000, 2.seconds)


    "do nothing if tx db becomes full" in {

      val smallSizedUTXSettings = UTXSettings(1, 1.second)

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
