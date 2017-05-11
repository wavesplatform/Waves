package scorex.transaction.state.database

import com.wavesplatform.settings.UTXSettings
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers, OneInstancePerTest}
import scorex.account.PublicKeyAccount
import scorex.transaction.ValidationError.{TransactionParameterValidationError, TransactionValidationError}
import scorex.transaction.{GenesisTransaction, Transaction}

import scala.concurrent.duration._

class UnconfirmedTransactionsDatabaseImplSpecification extends FreeSpec
  with Matchers
  with MockFactory
  with OneInstancePerTest {

  "utx database" - {

    val validDelegate = (tx: Transaction) => Right(tx)
    val invalidDelegate = (tx: Transaction) => Left(TransactionValidationError(tx, "Utx is full"))
    val defaultSizedUtxSettings = UTXSettings(1000, 2.seconds)


    "do nothing if tx db becomes full" in {

      val smallSizedUTXSettings = UTXSettings(1, 1.second)

      val db = new UnconfirmedTransactionsDatabaseImpl(smallSizedUTXSettings.size)


      db.putIfNew(newTx(1), validDelegate) shouldBe an[Right[_, _]]

      db.all() should have size 1

      db.putIfNew(newTx(2), validDelegate) shouldBe an[Left[_, _]]
    }

    "does not call validator if same tx comes again" in {
      val db = new UnconfirmedTransactionsDatabaseImpl(defaultSizedUtxSettings.size)

      db.putIfNew(newTx(1), validDelegate) shouldBe an[Right[_, _]]
      db.putIfNew(newTx(1), validDelegate) shouldBe an[Left[_, _]]
    }

    "validator returns false" in {
      val db = new UnconfirmedTransactionsDatabaseImpl(defaultSizedUtxSettings.size)


      db.all() shouldBe empty

      db.putIfNew(newTx(1), invalidDelegate) shouldBe an[Left[_, _]]
      db.all() shouldBe empty
    }
  }

  private def newTx(id: Long) = GenesisTransaction.create(PublicKeyAccount(Array.fill(32)(0: Byte)), id, 4598723454L).right.get

}
