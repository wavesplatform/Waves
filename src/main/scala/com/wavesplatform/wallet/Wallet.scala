package com.wavesplatform.wallet

import java.io.File

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.account.{Address, PrivateKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.settings.WalletSettings
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.MissingSenderPrivateKey
import com.wavesplatform.utils.{JsonFileStorage, randomBytes, _}
import play.api.libs.json._

import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success, Try}

trait Wallet {
  def seed: Array[Byte]
  def nonce: Int
  def privateKeyAccounts: List[PrivateKeyAccount]
  def generateNewAccounts(howMany: Int): Seq[PrivateKeyAccount]
  def generateNewAccount(): Option[PrivateKeyAccount]
  def deleteAccount(account: PrivateKeyAccount): Boolean
  def privateKeyAccount(account: Address): Either[ValidationError, PrivateKeyAccount]
}

object Wallet extends ScorexLogging {
  implicit class WalletExtension(private val wallet: Wallet) extends AnyVal {
    def findPrivateKey(addressString: String): Either[ValidationError, PrivateKeyAccount] =
      for {
        acc        <- Address.fromString(addressString)
        privKeyAcc <- wallet.privateKeyAccount(acc)
      } yield privKeyAcc

    def exportAccountSeed(account: Address): Either[ValidationError, Array[Byte]] =
      wallet.privateKeyAccount(account).map(_.seed)
  }

  def generateNewAccount(seed: Array[Byte], nonce: Int): PrivateKeyAccount = {
    val accountSeed = generateAccountSeed(seed, nonce)
    PrivateKeyAccount(accountSeed)
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

    private[this] val accountsCache: TrieMap[String, PrivateKeyAccount] = {
      val accounts = walletData.accountSeeds.map(seed => PrivateKeyAccount(seed.arr))
      TrieMap(accounts.map(acc => acc.address -> acc).toSeq: _*)
    }

    override def seed: Array[Byte] =
      this.walletData.seed.arr

    override def privateKeyAccounts: List[PrivateKeyAccount] =
      this.accountsCache.values.toList

    override def generateNewAccounts(howMany: Int): Seq[PrivateKeyAccount] =
      (1 to howMany)
        .flatMap(_ => this.generateNewAccountWithoutSave())
        .tap(_ => this.saveWalletFile())

    override def generateNewAccount(): Option[PrivateKeyAccount] = WalletLock.write {
      generateNewAccountWithoutSave().map(acc => {
        this.saveWalletFile()
        acc
      })
    }

    override def deleteAccount(account: PrivateKeyAccount): Boolean = WalletLock.write {
      val before = walletData.accountSeeds.size
      walletData = walletData.copy(accountSeeds = walletData.accountSeeds - ByteStr(account.seed))
      accountsCache -= account.address
      this.saveWalletFile()
      before > walletData.accountSeeds.size
    }

    override def privateKeyAccount(account: Address): Either[ValidationError, PrivateKeyAccount] =
      accountsCache.get(account.address).toRight[ValidationError](MissingSenderPrivateKey)

    override def nonce: Int =
      walletData.nonce

    private[this] def saveWalletFile(): Unit =
      maybeFile.foreach(f => JsonFileStorage.save(walletData, f.getCanonicalPath, Some(encryptionKey)))

    private[this] def generateNewAccountWithoutSave(): Option[PrivateKeyAccount] = WalletLock.write {
      val nonce   = getAndIncrementNonce()
      val account = Wallet.generateNewAccount(seed, nonce)

      val address = account.address
      if (!accountsCache.contains(address)) {
        accountsCache += account.address -> account
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
