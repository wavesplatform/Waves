package scorex.wallet

import java.io.File
import java.nio.charset.MalformedInputException

import com.fasterxml.jackson.databind.JsonMappingException
import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.settings.WalletSettings
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.utils.JsonFileStorage
import play.api.libs.json._
import scorex.account.{Address, PrivateKeyAccount}
import scorex.crypto.encode.Base64
import scorex.crypto.hash.SecureCryptographicHash
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.MissingSenderPrivateKey
import scorex.utils.{randomBytes, ScorexLogging}

import scala.collection.concurrent.TrieMap

trait Wallet extends AutoCloseable{

  def seed: Array[Byte]

  def nonce: Int

  def privateKeyAccounts: List[PrivateKeyAccount]

  def generateNewAccounts(howMany: Int): Seq[PrivateKeyAccount]

  def generateNewAccount(): Option[PrivateKeyAccount]

  def deleteAccount(account: PrivateKeyAccount): Boolean

  def privateKeyAccount(account: Address): Either[ValidationError, PrivateKeyAccount]

  def close(): Unit

}

object Wallet extends ScorexLogging {

  implicit class WalletExtension(w: Wallet) {
    def findWallet(addressString: String): Either[ValidationError, PrivateKeyAccount] = for {
      acc <- Address.fromString(addressString)
      privKeyAcc <- w.privateKeyAccount(acc)
    } yield privKeyAcc

    def exportAccountSeed(account: Address): Either[ValidationError, Array[Byte]] = w.privateKeyAccount(account).map(_.seed)
  }

  def generateNewAccount(seed: Array[Byte], nonce: Int): PrivateKeyAccount = {
    val accountSeed = generateAccountSeed(seed, nonce)
    PrivateKeyAccount(accountSeed)
  }

  def generateAccountSeed(seed: Array[Byte], nonce: Int): Array[Byte] =
    SecureCryptographicHash(Bytes.concat(Ints.toByteArray(nonce), seed))

  def apply(settings: WalletSettings): Wallet = {

    val seed = settings.seed.flatMap(bytes => if (bytes.arr.isEmpty) None else Some(bytes)).orElse {
      val SeedSize = 64
      val randomSeed = ByteStr(randomBytes(SeedSize))
      log.info(s"You random generated seed is ${randomSeed.base58}")
      Some(randomSeed)
    }.get.arr

    new WalletImpl(settings.file, settings.password, seed)
  }

  class WalletImpl private[Wallet](file: Option[File], password: String, s: Array[Byte]) extends ScorexLogging with Wallet {
    private type AccountSeed = Array[Byte]

    private case class WalletData(seed: AccountSeed,
                                  accountSeeds: Set[AccountSeed],
                                  nonce: Int)

    implicit val arrFormat: Format[AccountSeed] = Format[AccountSeed](
      js => JsSuccess(Base64.decode(js.as[JsString].value)),
      array => JsString(Base64.encode(array))
    )
    private implicit val format: OFormat[WalletData] = Json.format[WalletData]

    private var walletData = file.filter(_.exists()).flatMap(f =>
      try Some(JsonFileStorage.load[WalletData](f.getCanonicalPath, Option(password)))
      catch {
        case _: MalformedInputException | _: JsonMappingException => Option(migrateFromOldWallet())
      }
    ).getOrElse(WalletData(s, Set.empty, 0))

    private def migrateFromOldWallet(): WalletData = {
      val oldWallet = new WalletObsolete(file, password.toCharArray, Some(s))
      val walletData = WalletData(oldWallet.seed, oldWallet.privateKeyAccounts().map(a => a.seed).toSet, oldWallet.nonce())
      oldWallet.close()
      walletData
    }

    private val l = new Object

    private def lock[T](f: => T): T = l.synchronized(f)

    private val accountsCache: TrieMap[String, PrivateKeyAccount] = {
      val accounts = walletData.accountSeeds.map(seed => PrivateKeyAccount(seed))
      TrieMap(accounts.map(acc => acc.address -> acc).toSeq: _*)
    }

    private def save(): Unit = file.foreach(f => JsonFileStorage.save(walletData, f.getCanonicalPath, Option(password)))

    override def seed: Array[Byte] = walletData.seed

    override def privateKeyAccounts: List[PrivateKeyAccount] = accountsCache.values.toList

    override def generateNewAccounts(howMany: Int): Seq[PrivateKeyAccount] =
      (1 to howMany).flatMap(_ => generateNewAccount())

    override def generateNewAccount(): Option[PrivateKeyAccount] = lock {
      val nonce = getAndIncrementNonce()
      val account = Wallet.generateNewAccount(seed, nonce)

      val address = account.address
      val created = if (!accountsCache.contains(address)) {
        accountsCache += account.address -> account
        walletData = walletData.copy(accountSeeds = walletData.accountSeeds + account.seed)
        save()
        true
      } else false

      if (created) {
        log.info("Added account #" + privateKeyAccounts.size)
        Some(account)
      } else None
    }

    override def deleteAccount(account: PrivateKeyAccount): Boolean = lock {
      val before = walletData.accountSeeds.size
      walletData = walletData.copy(accountSeeds = walletData.accountSeeds - account.seed)
      accountsCache -= account.address
      save()
      before > walletData.accountSeeds.size
    }

    override def privateKeyAccount(account: Address): Either[ValidationError, PrivateKeyAccount] =
      accountsCache.get(account.address).toRight[ValidationError](MissingSenderPrivateKey)

    override def close(): Unit = {
      save()
      accountsCache.clear()
    }

    override def nonce: Int = walletData.nonce

    private def getAndIncrementNonce(): Int = lock {
      val r = walletData.nonce
      walletData = walletData.copy(nonce = walletData.nonce + 1)
      r
    }
  }
}
