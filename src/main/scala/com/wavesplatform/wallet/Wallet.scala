package com.wavesplatform.wallet

import java.io.File

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.account.{KeyPair, Address}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.settings.WalletSettings
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.MissingSenderPrivateKey
import com.wavesplatform.utils.{JsonFileStorage, randomBytes, _}
import play.api.libs.json._

import scala.collection.concurrent.TrieMap
import scala.util.control.NonFatal

trait Wallet {
  def seed: Array[Byte]
  def nonce: Int
  def PrivateKeys: Seq[KeyPair]
  def generateNewAccounts(howMany: Int): Seq[KeyPair]
  def generateNewAccount(): Option[KeyPair]
  def deleteAccount(account: KeyPair): Boolean
  def PrivateKey(account: Address): Either[ValidationError, KeyPair]
}

object Wallet extends ScorexLogging {

  implicit class WalletExtension(w: Wallet) {
    def findPrivateKey(addressString: String): Either[ValidationError, KeyPair] =
      for {
        acc        <- Address.fromString(addressString)
        privKeyAcc <- w.PrivateKey(acc)
      } yield privKeyAcc

    def exportAccountSeed(account: Address): Either[ValidationError, Array[Byte]] = w.PrivateKey(account).map(_.seed)
  }

  private case class WalletData(seed: ByteStr, accountSeeds: Set[ByteStr], nonce: Int)

  private implicit val walletFormat: Format[WalletData] = Json.format

  def generateNewAccount(seed: Array[Byte], nonce: Int): KeyPair = {
    val accountSeed = generateAccountSeed(seed, nonce)
    KeyPair(accountSeed)
  }

  def generateAccountSeed(seed: Array[Byte], nonce: Int): Array[Byte] =
    crypto.secureHash(Bytes.concat(Ints.toByteArray(nonce), seed))

  def apply(settings: WalletSettings): Wallet = new WalletImpl(settings.file, settings.password, settings.seed)

  private class WalletImpl(maybeFile: Option[File], passwordOpt: Option[String], maybeSeedFromConfig: Option[ByteStr])
      extends ScorexLogging
      with Wallet {

    private lazy val key = {
      val password = passwordOpt.getOrElse(PasswordProvider.askPassword())
      JsonFileStorage.prepareKey(password)
    }

    private def loadOrImport(f: File): Option[WalletData] =
      try {
        Some(JsonFileStorage.load[WalletData](f.getCanonicalPath, Some(key)))
      } catch {
        case NonFatal(_) => None
      }

    private lazy val actualSeed = maybeSeedFromConfig.getOrElse {
      val randomSeed = ByteStr(randomBytes(64))
      log.info(s"Your randomly generated seed is ${randomSeed.base58}")
      randomSeed
    }

    private var walletData: WalletData = {
      if (maybeFile.isEmpty)
        WalletData(actualSeed, Set.empty, 0)
      else {
        val file = maybeFile.get
        if (file.exists() && file.length() > 0) {
          val wd = loadOrImport(maybeFile.get)
          if (wd.isDefined) wd.get
          else {
            throw new IllegalStateException(s"Failed to open existing wallet file '${maybeFile.get}' maybe provided password is incorrect")
          }
        } else WalletData(actualSeed, Set.empty, 0)
      }
    }

    private val l = new Object

    private def lock[T](f: => T): T = l.synchronized(f)

    private[this] val accountsCache: TrieMap[String, KeyPair] = {
      val accounts = walletData.accountSeeds.map(KeyPair(_))
      TrieMap(accounts.map(acc => acc.address -> acc).toSeq: _*)
    }

    private def save(): Unit = maybeFile.foreach(f => JsonFileStorage.save(walletData, f.getCanonicalPath, Some(key)))

    private def generateNewAccountWithoutSave(): Option[KeyPair] = lock {
      val nonce   = getAndIncrementNonce()
      val account = Wallet.generateNewAccount(seed, nonce)

      val address = account.address
      if (!accountsCache.contains(address)) {
        accountsCache += account.address -> account
        walletData = walletData.copy(accountSeeds = walletData.accountSeeds + ByteStr(account.seed))
        log.info("Added account #" + this.PrivateKeys.length)
        Some(account)
      } else None
    }

    override def seed: Array[Byte] = walletData.seed.arr

    override def PrivateKeys: Seq[KeyPair] =
      accountsCache.values.toVector

    override def generateNewAccounts(howMany: Int): Seq[KeyPair] =
      (1 to howMany).flatMap(_ => generateNewAccountWithoutSave()).tap(_ => save())

    override def generateNewAccount(): Option[KeyPair] = lock {
      generateNewAccountWithoutSave().map(acc => {
        save()
        acc
      })
    }

    override def deleteAccount(account: KeyPair): Boolean = lock {
      val before = walletData.accountSeeds.size
      walletData = walletData.copy(accountSeeds = walletData.accountSeeds - ByteStr(account.seed))
      accountsCache -= account.address
      save()
      before > walletData.accountSeeds.size
    }

    override def PrivateKey(account: Address): Either[ValidationError, KeyPair] =
      accountsCache.get(account.address).toRight[ValidationError](MissingSenderPrivateKey)
    override def nonce: Int = walletData.nonce

    private def getAndIncrementNonce(): Int = lock {
      val r = walletData.nonce
      walletData = walletData.copy(nonce = walletData.nonce + 1)
      r
    }
  }

}
