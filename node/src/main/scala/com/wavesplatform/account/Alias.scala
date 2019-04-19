package com.wavesplatform.account
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.TxValidationError.GenericError

sealed trait Alias extends AddressOrAlias {
  lazy val stringRepr: String = Alias.Prefix + chainId.toChar + ":" + name
  lazy val bytes: ByteStr     = ByteStr(Alias.AddressVersion +: chainId +: Deser.serializeArray(name.getBytes("UTF-8")))

  val name: String
  val chainId: Byte
}

object Alias {
  val Prefix: String = "alias:"

  val AddressVersion: Byte = 2
  val MinLength            = 4
  val MaxLength            = 30

  val AliasAlphabet = "-.0123456789@_abcdefghijklmnopqrstuvwxyz"

  def create(name: String): Either[ValidationError, Alias] = {
    createWithChainId(name, currentChainId)
  }

  def fromString(str: String): Either[ValidationError, Alias] = {
    val aliasPatternInfo = "Alias string pattern is 'alias:<chain-id>:<address-alias>"

    if (!str.startsWith(Prefix)) {
      Left(GenericError(aliasPatternInfo))
    } else {
      val charSemicolonAlias = str.drop(Prefix.length)
      val chainId            = charSemicolonAlias(0).toByte
      val name               = charSemicolonAlias.drop(2)
      if (charSemicolonAlias(1) != ':') {
        Left(GenericError(aliasPatternInfo))
      } else {
        createWithChainId(name, chainId)
      }
    }
  }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, Alias] = {
    bytes match {
      case Array(`AddressVersion`, chainId, _, _, rest @ _*) =>
        createWithChainId(new String(rest.toArray, "UTF-8"), chainId)

      case _ =>
        Left(GenericError("Bad alias bytes"))
    }
  }

  @inline private[this] def currentChainId: Byte = AddressScheme.current.chainId
  private[this] def isValidAliasChar(c: Char): Boolean =
    ('0' <= c && c <= '9') || ('a' <= c && c <= 'z') || c == '_' || c == '@' || c == '-' || c == '.'

  private[wavesplatform] def createWithChainId(name: String, chainId: Byte = currentChainId): Either[ValidationError, Alias] = {
    final case class AliasImpl(chainId: Byte, name: String) extends Alias

    if (name.length < MinLength || MaxLength < name.length)
      Left(GenericError(s"Alias '$name' length should be between $MinLength and $MaxLength"))
    else if (!name.forall(isValidAliasChar))
      Left(GenericError(s"Alias should contain only following characters: $AliasAlphabet"))
    else if (chainId != 0 && chainId != currentChainId)
      Left(GenericError("Alias network char doesn't match current scheme"))
    else
      Right(AliasImpl(if (chainId == 0) currentChainId else chainId, name))
  }
}
