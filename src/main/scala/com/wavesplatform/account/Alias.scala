package com.wavesplatform.account

import com.wavesplatform.state.ByteStr
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.GenericError

sealed trait Alias extends AddressOrAlias {
  lazy val stringRepr: String = Alias.Prefix + networkByte.toChar + ":" + name
  lazy val bytes: ByteStr     = ByteStr(Alias.AddressVersion +: networkByte +: Deser.serializeArray(name.getBytes("UTF-8")))

  val name: String
  val networkByte: Byte
}

object Alias {

  val Prefix: String = "alias:"

  val AddressVersion: Byte = 2
  val MinLength            = 4
  val MaxLength            = 30

  val aliasAlphabet = "-.0123456789@_abcdefghijklmnopqrstuvwxyz"

  private val AliasPatternInfo = "Alias string pattern is 'alias:<chain-id>:<address-alias>"

  private def schemeByte: Byte = AddressScheme.current.chainId

  private def validAliasChar(c: Char): Boolean =
    ('0' <= c && c <= '9') || ('a' <= c && c <= 'z') || c == '_' || c == '@' || c == '-' || c == '.'

  private def buildAlias(networkByte: Byte, name: String): Either[ValidationError, Alias] = {

    case class AliasImpl(networkByte: Byte, name: String) extends Alias

    if (name.length < MinLength || MaxLength < name.length)
      Left(GenericError(s"Alias '$name' length should be between $MinLength and $MaxLength"))
    else if (!name.forall(validAliasChar))
      Left(GenericError(s"Alias should contain only following characters: $aliasAlphabet"))
    else if (networkByte != schemeByte)
      Left(GenericError("Alias network char doesn't match current scheme"))
    else
      Right(AliasImpl(networkByte, name))
  }

  def buildWithCurrentNetworkByte(name: String): Either[ValidationError, Alias] = buildAlias(schemeByte, name)

  def fromString(str: String): Either[ValidationError, Alias] =
    if (!str.startsWith(Prefix)) {
      Left(GenericError(AliasPatternInfo))
    } else {
      val charSemicolonAlias = str.drop(Prefix.length)
      val networkByte        = charSemicolonAlias(0).toByte
      val name               = charSemicolonAlias.drop(2)
      if (charSemicolonAlias(1) != ':') {
        Left(GenericError(AliasPatternInfo))
      } else {
        buildAlias(networkByte, name)
      }
    }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, Alias] = {
    bytes.headOption match {
      case Some(AddressVersion) =>
        val networkChar = bytes.tail.head
        if (networkChar != schemeByte) {
          Left(GenericError("Alias network byte doesn't match current scheme"))
        } else
          buildAlias(networkChar, new String(bytes.drop(4), "UTF-8"))
      case _ => Left(GenericError("Bad alias bytes"))
    }
  }
}
