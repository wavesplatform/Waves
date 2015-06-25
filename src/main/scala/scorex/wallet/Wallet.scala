package scorex.wallet

import java.io.File
import java.util.logging.Logger

import com.google.common.primitives.{Bytes, Ints}
import org.mapdb.{DBMaker, Serializer}
import scorex.account.PrivateKeyAccount
import scorex.crypto.Crypto

import scala.collection.JavaConversions._
import scala.collection.concurrent.TrieMap
import scala.util.Try


class Wallet(walletFile: File, password: String, seed: Array[Byte]) {

  import Wallet._

  //create parent folders then check their existence
  walletFile.getParentFile.mkdirs().ensuring(walletFile.getParentFile.exists())

  private val database = DBMaker.newFileDB(walletFile)
    .encryptionEnable(password)
    .checksumEnable()
    .closeOnJvmShutdown()
    .make()

  Try(database.createAtomicVar(SEED, seed, Serializer.BYTE_ARRAY))
    .getOrElse(database.getAtomicVar(SEED).set(seed))

  private lazy val accountsPersistence = database.createHashSet("accounts").makeOrGet[Array[Byte]]()

  private lazy val accountsCache: TrieMap[String, PrivateKeyAccount] = {
    val accs = accountsPersistence.map(seed => new PrivateKeyAccount(seed))
    TrieMap(accs.map(acc => acc.address -> acc).toSeq: _*)
  }

  def privateKeyAccounts(): Seq[PrivateKeyAccount] = accountsCache.values.toSeq

  def generateNewAccounts(howMany: Int): Seq[PrivateKeyAccount] =
    (1 to howMany).flatMap(_ => generateNewAccount())

  def generateNewAccount(): Option[PrivateKeyAccount] = synchronized {
    //READ NONCE
    val nonce = getAndIncrementNonce()

    //GENERATE ACCOUNT SEED
    val accountSeed = generateAccountSeed(seed, nonce)
    val account = new PrivateKeyAccount(accountSeed)

    val address = account.address
    val created = if (!accountsCache.containsKey(address)) {
      accountsCache += account.address -> account
      accountsPersistence.add(account.seed)
      database.commit()
      true
    } else false

    if (created) {
      Logger.getGlobal.info("Added account #" + nonce)
      Some(account)
    } else None
  }

  def generateAccountSeed(seed: Array[Byte], nonce: Int): Array[Byte] = {
    val nonceBytes = Ints.toByteArray(nonce)
    val accountSeed = Bytes.concat(nonceBytes, seed, nonceBytes)
    Crypto.doubleSha256(accountSeed)
  }

  def deleteAccount(account: PrivateKeyAccount): Boolean = synchronized {
    val res = accountsPersistence.remove(account.seed)
    database.commit()
    accountsCache -= account.address
    res
  }

  def exportAccountSeed(address: String): Option[Array[Byte]] = privateKeyAccount(address).map(_.seed)

  def privateKeyAccount(address: String) = accountsCache.get(address)

  def exportSeed(): Array[Byte] = database.getAtomicVar(SEED).get()

  def close() = if (!database.isClosed) {
    database.commit()
    database.close()
    accountsCache.clear()
  }

  def exists() = walletFile.exists()

  def accounts() = accountsCache.values.toSeq

  def nonce(): Int = database.getAtomicInteger(NONCE).intValue()

  def setNonce(nonce: Int) = database.getAtomicInteger(NONCE).set(nonce)

  def getAndIncrementNonce(): Int = database.getAtomicInteger(NONCE).getAndIncrement()
}


object Wallet {
  private val SEED = "seed"
  private val NONCE = "nonce"
}