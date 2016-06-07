package scorex.wallet

import java.io.File

import com.google.common.primitives.{Bytes, Ints}
import org.h2.mvstore.{MVMap, MVStore}
import scorex.account.PrivateKeyAccount
import scorex.crypto.encode.Base58
import scorex.crypto.hash.SecureCryptographicHash
import scorex.utils.ScorexLogging

import scala.collection.JavaConversions._
import scala.collection.concurrent.TrieMap
import scorex.utils.randomBytes

//todo: add accs txs?
class Wallet(walletFileOpt: Option[File], password: String, seedOpt: Option[Array[Byte]]) extends ScorexLogging {

  private val NonceFieldName = "nonce"

  private val database: MVStore = walletFileOpt match {
    case Some(walletFile) =>
      //create parent folders then check their existence
      walletFile.getParentFile.mkdirs().ensuring(walletFile.getParentFile.exists())
      new MVStore.Builder().fileName(walletFile.getAbsolutePath).encryptionKey(password.toCharArray).compress().open()

    case None => new MVStore.Builder().open()
  }


  private val accountsPersistence: MVMap[Int, Array[Byte]] = database.openMap("privkeys")
  private val seedPersistence: MVMap[String, Array[Byte]] = database.openMap("seed")
  private val noncePersistence: MVMap[String, Int] = database.openMap("nonce")

  if (Option(seedPersistence.get("seed")).isEmpty) {
    val seed = seedOpt.getOrElse {
      val Attempts = 10
      val SeedSize = 64
      lazy val randomSeed = randomBytes(SeedSize)
      lazy val encodedSeed = Base58.encode(randomSeed)
      def readSeed(limit: Int = Attempts): Array[Byte] = {
        println("Please type your wallet seed or type Enter to generate random one")
        val typed = scala.io.StdIn.readLine()
        if (typed == "") {
          println(s"You random generated seed is $encodedSeed")
          randomSeed
        } else
          Base58.decode(typed).getOrElse {
            if (limit > 0) {
              println("Wallet seed should be correct Base58 encoded string.")
              readSeed(limit - 1)
            } else throw new Error("Sorry you have made too many incorrect seed guesses")
          }
      }
      readSeed()
    }
    seedPersistence.put("seed", seed)
  }
  val seed: Array[Byte] = seedPersistence.get("seed")

  private val accountsCache: TrieMap[String, PrivateKeyAccount] = {
    val accs = accountsPersistence.keys.map(k => accountsPersistence.get(k)).map(seed => new PrivateKeyAccount(seed))
    TrieMap(accs.map(acc => acc.address -> acc).toSeq: _*)
  }

  def privateKeyAccounts(): Seq[PrivateKeyAccount] = accountsCache.values.toSeq

  def generateNewAccounts(howMany: Int): Seq[PrivateKeyAccount] =
    (1 to howMany).flatMap(_ => generateNewAccount())

  def generateNewAccount(): Option[PrivateKeyAccount] = synchronized {
    val nonce = getAndIncrementNonce()

    val accountSeed = generateAccountSeed(seed, nonce)
    val account = new PrivateKeyAccount(accountSeed)

    val address = account.address
    val created = if (!accountsCache.containsKey(address)) {
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

  def generateAccountSeed(seed: Array[Byte], nonce: Int): Array[Byte] =
    SecureCryptographicHash(Bytes.concat(Ints.toByteArray(nonce), seed))


  def deleteAccount(account: PrivateKeyAccount): Boolean = synchronized {
    val res = accountsPersistence.keys.find { k =>
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

  def exists(): Boolean = walletFileOpt.map(_.exists()).getOrElse(true)

  def nonce(): Int = Option(noncePersistence.get(NonceFieldName)).getOrElse(0)

  def getAndIncrementNonce(): Int = synchronized {
    noncePersistence.put(NonceFieldName, nonce() + 1)
  }

}
