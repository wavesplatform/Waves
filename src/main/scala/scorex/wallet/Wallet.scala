package scorex.wallet

import java.util.concurrent.atomic.AtomicReference
import java.util.logging.Logger
import com.google.common.primitives.Bytes
import com.google.common.primitives.Ints
import controller.Controller
import database.wallet.SecureWalletDatabase
import scorex.account.PrivateKeyAccount
import scorex.crypto.Crypto
import scala.util.Try


object Wallet {
  val STATUS_UNLOCKED = 1
  val STATUS_LOCKED = 0

  private val secureDatabaseRef: AtomicReference[Option[SecureWalletDatabase]] = new AtomicReference(None)

  private def secureDb() = secureDatabaseRef.get()

  def isUnlocked() = secureDb().isDefined

  def privateKeyAccounts() = secureDatabaseRef.get match {
    case None => Seq[PrivateKeyAccount]()
    case Some(secureDatabase) => secureDatabase.accounts()
  }

  def privateKeyAccount(address: String) = secureDb().flatMap(_.account(address))

  //CREATE

  def create(seed: Array[Byte], password: String, depth: Int, synchronize: Boolean): Boolean = {
    //OPEN SECURE WALLET
    val secureDatabase = new SecureWalletDatabase(password)

    //CREATE
    create(secureDatabase, seed, depth, synchronize)
  }

  def create(secureDatabase: SecureWalletDatabase, seed: Array[Byte], depth: Int, sync: Boolean): Boolean = {
    //CREATE SECURE WALLET
    secureDatabaseRef.set(Some(secureDatabase))

    //ADD SEED
    secureDatabase.setSeed(seed)

    //ADD NONCE
    secureDatabase.setNonce(0)

    //CREATE ACCOUNTS
    (1 to depth).foreach(_ => generateNewAccount())

    //COMMIT
    commit()

    true
  }

  def generateNewAccount(): Option[PrivateKeyAccount] = secureDb().map { db =>
    //READ SEED
    val seed = db.getSeed

    //READ NONCE
    val nonce = db.getAndIncrementNonce

    //GENERATE ACCOUNT SEED
    val accountSeed = generateAccountSeed(seed, nonce)
    val account = new PrivateKeyAccount(accountSeed)

    //CHECK IF ACCOUNT ALREADY EXISTS
    if (db.addAccount(account)) {
      Logger.getGlobal.info("Added account #" + nonce)
    }

    account
  }

  def generateAccountSeed(seed: Array[Byte], nonce: Int) = {
    val nonceBytes = Ints.toByteArray(nonce)
    val accountSeed = Bytes.concat(nonceBytes, seed, nonceBytes)
    Crypto.doubleSha256(accountSeed)
  }

  //DELETE
  def deleteAccount(account: PrivateKeyAccount) = {
    //CHECK IF WALLET IS OPEN
    if (!isUnlocked()) {
      false
    } else {
      //DELETE FROM DATABASE
      secureDatabaseRef.get().get.delete(account)

      //RETURN
      true
    }
  }

  //UNLOCK

  def unlock(password: String): Boolean = {
    if (isUnlocked()) {
      false
    } else {
      Try {
        secureDatabaseRef.set(Some(new SecureWalletDatabase(password)))
      }.toOption.isDefined
    }
  }

  def lock() = secureDb().map { db =>
    db.commit()
    db.close()
    secureDatabaseRef.set(None)
  }.isDefined


  //IMPORT/EXPORT

  def importAccountSeed(accountSeed: Array[Byte]): Option[String] = secureDb().flatMap { db =>
    if (accountSeed.length != 32) {
      None
    } else {
      val account = new PrivateKeyAccount(accountSeed)
      if (db.addAccount(account)) Some(account.address) else None
    }
  }

  def exportAccountSeed(address: String): Option[Array[Byte]] = privateKeyAccount(address).map(_.seed)

  def exportSeed(): Option[Array[Byte]] = secureDb().map(_.getSeed())

  def close() {
    secureDb().map(_.close())
  }

  def commit() {
    secureDb().map(_.commit())
  }
}