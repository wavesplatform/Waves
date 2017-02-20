package scorex.wallet

import java.io.File

import com.google.common.primitives.{Bytes, Ints}
import org.h2.mvstore.{MVMap, MVStore}
import scorex.account.PrivateKeyAccount
import scorex.crypto.encode.Base58
import scorex.crypto.hash.SecureCryptographicHash
import scorex.utils.{LogMVMapBuilder, ScorexLogging, randomBytes}

import scala.collection.JavaConverters._
import scala.collection.concurrent.TrieMap

//todo: add accs txs?
class Wallet(maybeFilename: Option[String], password: String, seedOpt: Option[Array[Byte]]) extends ScorexLogging {

  private val NonceFieldName = "nonce"

  private val database: MVStore = maybeFilename match {
    case Some(walletFilename) => {
      val walletFile = new File(walletFilename)
      walletFile.getParentFile.mkdirs().ensuring(walletFile.getParentFile.exists())

      new MVStore.Builder().fileName(walletFile.getAbsolutePath).encryptionKey(password.toCharArray).compress().open()
    }
    case None => new MVStore.Builder().open()
  }

  private val accountsPersistence: MVMap[Int, Array[Byte]] = database.openMap("privkeys", new LogMVMapBuilder[Int, Array[Byte]])
  private val seedPersistence: MVMap[String, Array[Byte]] = database.openMap("seed", new LogMVMapBuilder[String, Array[Byte]])
  private val noncePersistence: MVMap[String, Int] = database.openMap("nonce", new LogMVMapBuilder[String, Int])

  if (Option(seedPersistence.get("seed")).isEmpty) {
    val seed = seedOpt.getOrElse {
      val Attempts = 10
      val SeedSize = 64
      lazy val randomSeed = randomBytes(SeedSize)
      lazy val encodedSeed = Base58.encode(randomSeed)
      log.debug(s"You random generated seed is $encodedSeed")
      randomSeed
    }
    seedPersistence.put("seed", seed)
  }
  val seed: Array[Byte] = seedPersistence.get("seed")

  private val accountsCache: TrieMap[String, PrivateKeyAccount] = {
    val accounts = accountsPersistence.asScala.keys.map(k => accountsPersistence.get(k)).map(seed => new PrivateKeyAccount(seed))
    TrieMap(accounts.map(acc => acc.address -> acc).toSeq: _*)
  }

  def privateKeyAccounts(): List[PrivateKeyAccount] = accountsCache.values.toList

  def generateNewAccounts(howMany: Int): Seq[PrivateKeyAccount] =
    (1 to howMany).flatMap(_ => generateNewAccount())

  def generateNewAccount(): Option[PrivateKeyAccount] = synchronized {
    val nonce = getAndIncrementNonce()
    val account = Wallet.generateNewAccount(seed, nonce)

    val address = account.address
    val created = if (!accountsCache.contains(address)) {
      accountsCache += account.address -> account
      accountsPersistence.put(accountsPersistence.lastKey() + 1, account.seed)
      database.commit()
      true
    } else false

    if (created) {
      log.info("Added account #" + privateKeyAccounts().size)
      Some(account)
    } else None
  }

  def deleteAccount(account: PrivateKeyAccount): Boolean = synchronized {
    val res = accountsPersistence.asScala.keys.find { k =>
      if (accountsPersistence.get(k) sameElements account.seed) {
        accountsPersistence.remove(k)
        true
      } else false
    }
    database.commit()
    accountsCache -= account.address
    res.isDefined
  }

  def exportAccountSeed(address: String): Option[Array[Byte]] = privateKeyAccount(address).map(_.seed)

  def privateKeyAccount(address: String): Option[PrivateKeyAccount] = accountsCache.get(address)

  def close(): Unit = if (!database.isClosed) {
    database.commit()
    database.close()
    accountsCache.clear()
  }

  def nonce(): Int = Option(noncePersistence.get(NonceFieldName)).getOrElse(0)

  private def getAndIncrementNonce(): Int = synchronized {
    noncePersistence.put(NonceFieldName, nonce() + 1)
  }

}


object Wallet {

  def generateNewAccount(seed: Array[Byte], nonce: Int): PrivateKeyAccount = {
    val accountSeed = generateAccountSeed(seed, nonce)
    new PrivateKeyAccount(accountSeed)
  }

  def generateAccountSeed(seed: Array[Byte], nonce: Int): Array[Byte] =
    SecureCryptographicHash(Bytes.concat(Ints.toByteArray(nonce), seed))

}
