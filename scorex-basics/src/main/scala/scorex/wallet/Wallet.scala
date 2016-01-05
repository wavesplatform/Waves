package scorex.wallet

import java.io.File

import com.google.common.primitives.{Bytes, Ints}
import org.mapdb.{DBMaker, Serializer}
import scorex.account.PrivateKeyAccount
import scorex.crypto.hash.SecureCryptographicHash
import scorex.utils.ScorexLogging

import scala.collection.JavaConversions._
import scala.collection.concurrent.TrieMap

//todo: add accs txs?
class Wallet(walletFileOpt: Option[File],
             password: String,
             seed: Array[Byte]) extends ScorexLogging {

  private val NonceFieldName = "nonce"

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

  def generateAccountSeed(seed: Array[Byte], nonce: Int): Array[Byte] =
    SecureCryptographicHash(Bytes.concat(Ints.toByteArray(nonce), seed))


  def deleteAccount(account: PrivateKeyAccount): Boolean = synchronized {
    val res = accountsPersistence.remove(account.seed)
    database.commit()
    accountsCache -= account.address
    res
  }

  def exportAccountSeed(address: String): Option[Array[Byte]] = privateKeyAccount(address).map(_.seed)

  def privateKeyAccount(address: String): Option[PrivateKeyAccount] = accountsCache.get(address)

  def exportSeed(): Array[Byte] = seed

  def close(): Unit = if (!database.isClosed) {
    database.commit()
    database.close()
    accountsCache.clear()
  }

  def exists(): Boolean = walletFileOpt.map(_.exists()).getOrElse(true)

  def accounts(): Seq[PrivateKeyAccount] = accountsCache.values.toSeq

  def nonce(): Int = database.atomicInteger(NonceFieldName).intValue()

  def setNonce(nonce: Int): Unit = database.atomicInteger(NonceFieldName).set(nonce)

  def getAndIncrementNonce(): Int = database.atomicInteger(NonceFieldName).getAndIncrement()
}
