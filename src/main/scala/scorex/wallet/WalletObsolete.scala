package scorex.wallet

import java.io.File

import com.wavesplatform.utils.createMVStore
import org.h2.mvstore.MVMap
import scorex.account.{Address, PrivateKeyAccount}
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.MissingSenderPrivateKey
import scorex.utils.{LogMVMapBuilder, ScorexLogging}

import scala.collection.concurrent.TrieMap
import scala.collection.JavaConverters._

@deprecated("This is the old wallet version used for migration", "0.8.6")
class WalletObsolete (file: Option[File], password: Array[Char], s: Option[Array[Byte]]) extends AutoCloseable with ScorexLogging {

  private val NonceFieldName = "nonce"

  private val database = createMVStore(file, Some(password))

  private val accountsPersistence: MVMap[Int, Array[Byte]] = database.openMap("privkeys", new LogMVMapBuilder[Int, Array[Byte]])
  private val seedPersistence: MVMap[String, Array[Byte]] = database.openMap("seed", new LogMVMapBuilder[String, Array[Byte]])
  private val noncePersistence: MVMap[String, Int] = database.openMap("nonce", new LogMVMapBuilder[String, Int])

  if (Option(seedPersistence.get("seed")).isEmpty && s.isDefined)
    seedPersistence.put("seed", s.get)

  def seed: Array[Byte] = seedPersistence.get("seed")

  private val accountsCache: TrieMap[String, PrivateKeyAccount] = {
    val accounts = accountsPersistence.asScala.keys.map(k => accountsPersistence.get(k)).map(seed => PrivateKeyAccount(seed))
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

  def privateKeyAccount(account: Address): Either[ValidationError, PrivateKeyAccount] =
    accountsCache.get(account.address).toRight[ValidationError](MissingSenderPrivateKey)


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
