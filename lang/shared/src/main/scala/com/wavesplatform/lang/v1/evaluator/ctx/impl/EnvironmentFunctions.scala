package com.wavesplatform.lang.v1.evaluator.ctx.impl

import cats.implicits._
import cats.Monad
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.compiler.Terms.{CaseObj, CONST_BYTESTR, CONST_STRING}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.lang.v1.traits.domain.Recipient.{Address, Alias}

class EnvironmentFunctions[F[_]: Monad](environment: Environment[F]) {

  def toScala(addressOrAlias: CaseObj): Either[ExecutionError, Recipient] = {
    val objTypeName = addressOrAlias.caseType.name
    if (objTypeName == Types.addressType.name) {
      addressOrAlias.fields
        .get("bytes")
        .toRight(ExecutionError("Can't find 'bytes'"))
        .map(_.asInstanceOf[CONST_BYTESTR])
        .map(a => Address(a.bs))
    } else if (objTypeName == Types.aliasType.name) {
      addressOrAlias.fields
        .get("alias")
        .toRight(ExecutionError("Can't find alias"))
        .map(_.asInstanceOf[CONST_STRING])
        .map(a => Alias(a.s))
    } else {
      Left(ExecutionError(s"$addressOrAlias neither Address nor alias"))
    }
  }

  def getData(addressOrAlias: CaseObj, key: String, dataType: DataType): F[Either[ExecutionError, Option[Any]]] = {
    toScala(addressOrAlias).traverse(environment.data(_, key, dataType))
  }

  def hasData(addressOrAlias: CaseObj): F[Either[ExecutionError, Boolean]] = {
    toScala(addressOrAlias).traverse(environment.hasData)
  }

  def addressFromAlias(name: String): F[Either[ExecutionError, Recipient.Address]] =
    environment.resolveAlias(name)
}

object EnvironmentFunctions {
  val ChecksumLength       = 4
  val HashLength           = 20
  val AddressVersion: Byte = 1
  val AddressLength: Int   = 1 + 1 + ChecksumLength + HashLength
  val AddressStringLength  = 36
  val AddressPrefix        = "address:"
  val AliasVersion: Byte   = 2
}
