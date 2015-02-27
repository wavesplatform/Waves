package database.wallet

import java.io.File

import org.mapdb.{DBMaker, Serializer}
import scorex.account.PrivateKeyAccount
import settings.Settings

import scala.collection.JavaConversions._


class SecureWalletDatabase(password: String) {

  import SecureWalletDatabase._

  //OPEN WALLET
  SECURE_WALLET_FILE.getParentFile.mkdirs()

  private val database = DBMaker.newFileDB(SECURE_WALLET_FILE)
    .encryptionEnable(password)
    .closeOnJvmShutdown()
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

  def setSeed(seed: Array[Byte]) = database.createAtomicVar(SEED, seed, Serializer.BYTE_ARRAY)

  def getSeed(): Array[Byte] = database.getAtomicVar(SEED).get()

  def getNonce(): Int = database.getAtomicInteger(NONCE).intValue()

  def setNonce(nonce: Int) = database.getAtomicInteger(NONCE).set(nonce)

  def getAndIncrementNonce(): Int = database.getAtomicInteger(NONCE).getAndIncrement()

  def delete(account: PrivateKeyAccount) {
    accountsMap.remove(account.address)
  }

  def commit() = database.commit()

  def close() {
    if (!database.isClosed) {
      database.commit()
      database.close()
    }
  }
}

object SecureWalletDatabase {
  private val SECURE_WALLET_FILE = new File(Settings.walletDir, "wallet.s.dat")
  private val SEED = "seed"
  private val NONCE = "nonce"

  def exists() = SECURE_WALLET_FILE.exists()
}