package scorex.wallet

import java.util.logging.Logger

import com.google.common.primitives.{Bytes, Ints}
import scorex.account.PrivateKeyAccount
import scorex.crypto.Crypto
import scorex.database.wallet.SecureWalletDatabase
import scala.util.Try


//todo: the Wallet object is not thread-safe at all, fix!
object Wallet {
  private var secureDatabaseOpt: Option[SecureWalletDatabase] = None

  def privateKeyAccounts() = secureDatabaseOpt match {
    case None => Seq[PrivateKeyAccount]()
    case Some(secureDatabase) => secureDatabase.accounts()
  }

  def create(seed: Array[Byte], password: String, depth: Int): Boolean = {
    //OPEN SECURE WALLET
    val secureDatabase = new SecureWalletDatabase(password)

    //CREATE
    create(secureDatabase, seed, depth)
  }

  def create(secureDatabase: SecureWalletDatabase, seed: Array[Byte], depth: Int): Boolean = {
    //CREATE SECURE WALLET
    secureDatabaseOpt = Some(secureDatabase)

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

  def generateNewAccount(): Option[PrivateKeyAccount] = secureDatabaseOpt.map { db =>
    //READ SEED
    val seed = db.seed()

    //READ NONCE
    val nonce = db.getAndIncrementNonce()

    //GENERATE ACCOUNT SEED
    val accountSeed = generateAccountSeed(seed, nonce)
    val account = new PrivateKeyAccount(accountSeed)

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

  def commit() {
    secureDatabaseOpt.foreach(_.commit())
  }

  def deleteAccount(account: PrivateKeyAccount) = {
    //CHECK IF WALLET IS OPEN
    if (!isUnlocked) {
      false
    } else {
      //DELETE FROM DATABASE
      secureDatabaseOpt.get.delete(account)
      //RETURN
      true
    }
  }

  def isUnlocked = secureDatabaseOpt.isDefined

  def unlock(password: String): Boolean = {
    if (isUnlocked) {
      false
    } else {
      Try {
        secureDatabaseOpt = Some(new SecureWalletDatabase(password))
      }.toOption.isDefined
    }
  }

  def lock() = secureDatabaseOpt.map { db =>
    db.commit()
    db.close()
    secureDatabaseOpt = None
  }.isDefined


  //IMPORT/EXPORT

  def importAccountSeed(accountSeed: Array[Byte]): Option[String] = secureDatabaseOpt.flatMap { db =>
    if (accountSeed.length != 32) {
      None
    } else {
      val account = new PrivateKeyAccount(accountSeed)
      if (db.addAccount(account)) Some(account.address) else None
    }
  }

  def exportAccountSeed(address: String): Option[Array[Byte]] = privateKeyAccount(address).map(_.seed)

  def privateKeyAccount(address: String) = secureDatabaseOpt.flatMap(_.account(address))

  def exportSeed(): Option[Array[Byte]] = secureDatabaseOpt.map(_.seed())

  def close() {
    secureDatabaseOpt.map(_.close())
  }
}