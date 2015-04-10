package scorex.database.wallet

import java.io.File

import org.mapdb.{DBMaker, Serializer}
import scorex.account.PrivateKeyAccount
import settings.Settings

import scala.collection.JavaConversions._
import scala.util.Try


class SecureWalletDatabase(password: String, file: File) {

  import scorex.database.wallet.SecureWalletDatabase.{NONCE, SEED}

  file.getParentFile.mkdirs()

  private val database = DBMaker.newFileDB(file)
    .encryptionEnable(password)
    .cacheSize(2048)
    .checksumEnable()
    .mmapFileEnableIfSupported()
    .make()

  private val accountsMap = database.createHashMap("accounts").makeOrGet[String, PrivateKeyAccount]()

  def addAccount(account: PrivateKeyAccount) = {
    val address = account.address
    if (!accountsMap.containsKey(address)) {
      accountsMap.put(address, account)
      database.commit()
      true
    } else false
  }

  def accounts() = accountsMap.values().toSeq

  def account(address: String) = Option(accountsMap.get(address))

  def setSeed(seed: Array[Byte]): Unit = {
    Try(database.createAtomicVar(SEED, seed, Serializer.BYTE_ARRAY)).getOrElse(database.getAtomicVar(SEED).set(seed))
  }

  def seed(): Array[Byte] = database.getAtomicVar(SEED).get()

  def nonce(): Int = database.getAtomicInteger(NONCE).intValue()

  def setNonce(nonce: Int) = database.getAtomicInteger(NONCE).set(nonce)

  def getAndIncrementNonce(): Int = database.getAtomicInteger(NONCE).getAndIncrement()

  def delete(account: PrivateKeyAccount) {
    accountsMap.remove(account.address)
    database.commit()
  }

  def commit() = database.commit()

  def close() = this.synchronized {
    if (!database.isClosed) {
      database.commit()
      database.close()
    }
  }
}

object SecureWalletDatabase {
  private val SEED = "seed"
  private val NONCE = "nonce"
}