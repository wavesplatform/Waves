package scorex.transaction.state.wallet

import java.io.File

import com.google.common.primitives.{Bytes, Ints}
import org.mapdb.{Serializer, DBMaker}
import scorex.account.PrivateKeyAccount
import scorex.utils.ScorexLogging

import scala.collection.JavaConversions._
import scala.collection.concurrent.TrieMap
import scorex.crypto.Sha256._

//todo: XOR priv keys with seed in db?
class Wallet(walletFileOpt: Option[File], password: String, seed: Array[Byte]) extends ScorexLogging {

  private val NONCE = "nonce"

  private val database = walletFileOpt match {
    case Some(walletFile) =>
      //create parent folders then check their existence
      walletFile.getParentFile.mkdirs().ensuring(walletFile.getParentFile.exists())

      DBMaker.fileDB(walletFile)
        .checksumEnable()
        .closeOnJvmShutdown()
        .encryptionEnable(password)
        .make

    case None =>
      DBMaker.memoryDB().encryptionEnable(password).make
  }

  private val accountsPersistence = database.hashSet("privkeys", Serializer.BYTE_ARRAY)

  private val accountsCache: TrieMap[String, PrivateKeyAccount] = {
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
      log.info("Added account #" + nonce)
      Some(account)
    } else None
  }

  def generateAccountSeed(seed: Array[Byte], nonce: Int): Array[Byte] = {
    val nonceBytes = Ints.toByteArray(nonce)
    doubleHash(Bytes.concat(nonceBytes, seed, nonceBytes))
  }

  def deleteAccount(account: PrivateKeyAccount): Boolean = synchronized {
    val res = accountsPersistence.remove(account.seed)
    database.commit()
    accountsCache -= account.address
    res
  }

  def exportAccountSeed(address: String): Option[Array[Byte]] = privateKeyAccount(address).map(_.seed)

  def privateKeyAccount(address: String) = accountsCache.get(address)

  def exportSeed(): Array[Byte] = seed

  def close() = if (!database.isClosed) {
    database.commit()
    database.close()
    accountsCache.clear()
  }

  def exists() = walletFileOpt.map(_.exists()).getOrElse(true)

  def accounts() = accountsCache.values.toSeq

  def nonce(): Int = database.atomicInteger(NONCE).intValue()

  def setNonce(nonce: Int) = database.atomicInteger(NONCE).set(nonce)

  def getAndIncrementNonce(): Int = database.atomicInteger(NONCE).getAndIncrement()
}