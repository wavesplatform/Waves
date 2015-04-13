package scorex.database.wallet

import java.io.File

import org.mapdb.{DBMaker, Serializer}
import scorex.account.PrivateKeyAccount
import settings.Settings

import scala.collection.JavaConversions._
import scala.collection.concurrent.TrieMap
import scala.util.Try


class SecureWalletDatabase(password: String, file: File) {

  import scorex.database.wallet.SecureWalletDatabase.{NONCE, SEED}

  //create parent folders then check their existence
  file.getParentFile.mkdirs().ensuring(file.getParentFile.exists())

  private lazy val database = DBMaker.newFileDB(file)
    .encryptionEnable(password)
    .cacheSize(2048)
    .checksumEnable()
    .mmapFileEnableIfSupported()
    .make()

  private lazy val accountsPersistence = database.createHashSet("accounts").makeOrGet[Array[Byte]]()

  private lazy val accountsCache:TrieMap[String, PrivateKeyAccount] = {
    val accs = accountsPersistence.map(seed => new PrivateKeyAccount(seed))
    TrieMap(accs.map(acc => acc.address -> acc).toSeq :_*)
  }

  def addAccount(account: PrivateKeyAccount) = {
    val address = account.address
    if (!accountsCache.containsKey(address)) {
      accountsCache += account.address -> account
      accountsPersistence.add(account.seed)
      database.commit()
      true
    } else false
  }

  def accounts() = accountsCache.values.toSeq

  def account(address: String) = accountsCache.get(address)

  def setSeed(seed: Array[Byte]): Unit = {
    Try(database.createAtomicVar(SEED, seed, Serializer.BYTE_ARRAY)).getOrElse(database.getAtomicVar(SEED).set(seed))
  }

  def seed(): Array[Byte] = database.getAtomicVar(SEED).get()

  def nonce(): Int = database.getAtomicInteger(NONCE).intValue()

  def setNonce(nonce: Int) = database.getAtomicInteger(NONCE).set(nonce)

  def getAndIncrementNonce(): Int = database.getAtomicInteger(NONCE).getAndIncrement()

  def delete(account: PrivateKeyAccount) {
    accountsPersistence.remove(account.seed)
    database.commit()
    accountsCache -= account.address
  }

  def commit() = database.commit()

  def close() = this.synchronized {
    if (!database.isClosed) {
      database.commit()
      database.close()
      accountsCache.clear()
    }
  }
}

object SecureWalletDatabase {
  private val SEED = "seed"
  private val NONCE = "nonce"
}