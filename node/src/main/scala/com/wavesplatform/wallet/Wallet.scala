package com.wavesplatform.wallet

import java.io.File

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.WalletSettings
import com.wavesplatform.transaction.TxValidationError.MissingSenderPrivateKey
import com.wavesplatform.utils.{JsonFileStorage, randomBytes, _}
import play.api.libs.json._

import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success, Try}

trait Wallet {
  def seed: Array[Byte]
  def nonce: Int
  def privateKeyAccounts: Seq[KeyPair]
  def generateNewAccounts(howMany: Int): Seq[KeyPair]
  def generateNewAccount(): Option[KeyPair]
  def generateNewAccount(nonce: Int): Option[KeyPair]
  def deleteAccount(account: KeyPair): Boolean
  def privateKeyAccount(account: Address): Either[ValidationError, KeyPair]
}

object Wallet extends ScorexLogging {
  implicit class WalletExtension(private val wallet: Wallet) extends AnyVal {
    def findPrivateKey(addressString: String): Either[ValidationError, KeyPair] =
      for {
        acc        <- Address.fromString(addressString)
        privKeyAcc <- wallet.privateKeyAccount(acc)
      } yield privKeyAcc

    def exportAccountSeed(account: Address): Either[ValidationError, Array[Byte]] =
      wallet.privateKeyAccount(account).map(_.seed)
  }

  def generateNewAccount(seed: Array[Byte], nonce: Int): KeyPair = {
    val accountSeed = generateAccountSeed(seed, nonce)
    KeyPair(accountSeed)
  }

  def generateAccountSeed(seed: Array[Byte], nonce: Int): Array[Byte] =
    crypto.secureHash(Bytes.concat(Ints.toByteArray(nonce), seed))

  @throws[IllegalArgumentException]("if invalid wallet configuration provided")
  def apply(settings: WalletSettings): Wallet =
    new WalletImpl(settings.file, settings.password, settings.seed)

  private[this] final case class WalletData(seed: ByteStr, accountSeeds: Set[ByteStr], nonce: Int)

  private[this] object WalletData {
    implicit val walletFormat: Format[WalletData] = Json.format
  }

  private[this] final class WalletImpl(maybeFile: Option[File], passwordOpt: Option[String], maybeSeedFromConfig: Option[ByteStr])
      extends ScorexLogging
      with Wallet {

    private[this] lazy val encryptionKey = {
      val password = passwordOpt.getOrElse(PasswordProvider.askPassword())
      JsonFileStorage.prepareKey(password)
    }

    private[this] lazy val actualSeed = maybeSeedFromConfig.getOrElse {
      val randomSeed = ByteStr(randomBytes(64))
      log.info(s"Your randomly generated seed is ${randomSeed.base58}")
      randomSeed
    }

    private[this] var walletData: WalletData = {
      if (maybeFile.isEmpty)
        WalletData(actualSeed, Set.empty, 0)
      else {
        def loadOrImport(walletFile: File): Try[WalletData] =
          Try(JsonFileStorage.load[WalletData](walletFile.getCanonicalPath, Some(this.encryptionKey)))

        val file = maybeFile.get
        if (file.isFile && file.length() > 0) {
          loadOrImport(maybeFile.get) match {
            case Failure(exception) =>
              throw new IllegalArgumentException(s"Failed to open existing wallet file '${maybeFile.get}' maybe provided password is incorrect",
                                                 exception)
            case Success(value) => value
          }
        } else {
          WalletData(actualSeed, Set.empty, 0)
        }
      }
    }

    private[this] object WalletLock {
      private[this] val lockObject = new Object
      def write[T](f: => T): T     = lockObject.synchronized(f)
    }

    private[this] val accountsCache: TrieMap[String, KeyPair] = {
      val accounts = walletData.accountSeeds.map(KeyPair(_))
      TrieMap(accounts.map(acc => acc.stringRepr -> acc).toSeq: _*)
    }

    override def seed: Array[Byte] =
      this.walletData.seed.arr

    override def privateKeyAccounts: Seq[KeyPair] =
      this.accountsCache.values.toVector

    override def generateNewAccounts(howMany: Int): Seq[KeyPair] =
      (1 to howMany)
        .flatMap(_ => this.generateNewAccountWithoutSave())
        .tap(_ => this.saveWalletFile())

    override def generateNewAccount(): Option[KeyPair] = WalletLock.write {
      generateNewAccount(getAndIncrementNonce())
    }

    override def generateNewAccount(nonce: Int): Option[KeyPair] = WalletLock.write {
      generateNewAccountWithoutSave(nonce).map(acc => {
        this.saveWalletFile()
        acc
      })
    }

    override def deleteAccount(account: KeyPair): Boolean = WalletLock.write {
      val before = walletData.accountSeeds.size
      walletData = walletData.copy(accountSeeds = walletData.accountSeeds - ByteStr(account.seed))
      accountsCache -= account.stringRepr
      this.saveWalletFile()
      before > walletData.accountSeeds.size
    }

    override def privateKeyAccount(account: Address): Either[ValidationError, KeyPair] =
      accountsCache.get(account.stringRepr).toRight[ValidationError](MissingSenderPrivateKey)

    override def nonce: Int =
      walletData.nonce

    private[this] def saveWalletFile(): Unit =
      maybeFile.foreach(f => JsonFileStorage.save(walletData, f.getCanonicalPath, Some(encryptionKey)))

    private[this] def generateNewAccountWithoutSave(): Option[KeyPair] = WalletLock.write {
      generateNewAccountWithoutSave(getAndIncrementNonce())
    }

    private[this] def generateNewAccountWithoutSave(nonce: Int): Option[KeyPair] = WalletLock.write {
      val account = Wallet.generateNewAccount(seed, nonce)

      val address = account.stringRepr
      if (!accountsCache.contains(address)) {
        accountsCache += account.stringRepr -> account
        walletData = walletData.copy(accountSeeds = walletData.accountSeeds + ByteStr(account.seed))
        log.info("Added account #" + privateKeyAccounts.size)
        Some(account)
      } else None
    }

    private[this] def getAndIncrementNonce(): Int = WalletLock.write {
      val oldNonce = walletData.nonce
      walletData = walletData.copy(nonce = walletData.nonce + 1)
      oldNonce
    }
  }
}
