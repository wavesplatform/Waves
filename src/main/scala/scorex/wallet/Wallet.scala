package scorex.wallet

import java.io.File

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.settings.WalletSettings
import com.wavesplatform.utils.createMVStore
import org.h2.mvstore.MVMap
import scorex.account.{Account, PrivateKeyAccount}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.SecureCryptographicHash
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.MissingSenderPrivateKey
import scorex.utils.{LogMVMapBuilder, ScorexLogging, randomBytes}

import scala.collection.JavaConverters._
import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success}

class Wallet private(file: Option[File], password: Array[Char], seedOpt: Option[Array[Byte]])
  extends AutoCloseable with ScorexLogging {

  private val NonceFieldName = "nonce"

  private val database = createMVStore(file, Some(password))

  private val accountsPersistence: MVMap[Int, Array[Byte]] = database.openMap("privkeys", new LogMVMapBuilder[Int, Array[Byte]])
  private val seedPersistence: MVMap[String, Array[Byte]] = database.openMap("seed", new LogMVMapBuilder[String, Array[Byte]])
  private val noncePersistence: MVMap[String, Int] = database.openMap("nonce", new LogMVMapBuilder[String, Int])

  if (Option(seedPersistence.get("seed")).isEmpty) {
    val seed = seedOpt.getOrElse {
      val SeedSize = 64
      lazy val randomSeed = randomBytes(SeedSize)
      lazy val encodedSeed = Base58.encode(randomSeed)
      log.info(s"You random generated seed is $encodedSeed")
      randomSeed
    }
    seedPersistence.put("seed", seed)
  }
  val seed: Array[Byte] = seedPersistence.get("seed")

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

  def privateKeyAccount(account: Account): Either[ValidationError, PrivateKeyAccount] =
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


object Wallet {

  implicit class WalletExtension(w: Wallet) {
    def findWallet(addressString: String): Either[ValidationError, PrivateKeyAccount] = for {
      acc <- Account.fromString(addressString)
      privKeyAcc <- w.privateKeyAccount(acc)
    } yield privKeyAcc

    def exportAccountSeed(account: Account): Either[ValidationError, Array[Byte]] = w.privateKeyAccount(account).map(_.seed)
  }


  def generateNewAccount(seed: Array[Byte], nonce: Int): PrivateKeyAccount = {
    val accountSeed = generateAccountSeed(seed, nonce)
    PrivateKeyAccount(accountSeed)
  }

  def generateAccountSeed(seed: Array[Byte], nonce: Int): Array[Byte] =
    SecureCryptographicHash(Bytes.concat(Ints.toByteArray(nonce), seed))

  def apply(settings: WalletSettings): Wallet = {
    val seedOpt =
      if (settings.seed.isEmpty) None
      else Base58.decode(settings.seed) match {
        case Success(seed) => Some(seed)
        case Failure(f) =>
          throw new IllegalArgumentException("Invalid seed in config file, please, fix this", f)
      }
    new Wallet(settings.file, settings.password.toCharArray, seedOpt)
  }
}
