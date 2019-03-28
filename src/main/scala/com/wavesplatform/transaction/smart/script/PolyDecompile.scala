package com.wavesplatform.transaction.smart.script

import com.wavesplatform.lang.ContentType.{ContentType, DApp, Expression}
import com.wavesplatform.lang.ScriptType.{Account, Asset, ScriptType}
import com.wavesplatform.lang.StdLibVersion.{StdLibVersion, V1, V2, V3}
import shapeless.Poly1

object PolyDecompile extends Poly1 {
  implicit def decompile[A](implicit d: Decompile[A]) = at[A](d.decompile)
}

trait Decompile[A] {
  def decompile(value: A): (String, Any) = (key, decompileValue(value))
  def key: String
  def decompileValue(value: A): Any
}

object DecompileInstances {
  implicit val versionDecompiler = new Decompile[StdLibVersion] {
    override def key = "STDLIB_VERSION"
    override def decompileValue(value: StdLibVersion): Int = value match {
      case V1 => 1
      case V2 => 2
      case V3 => 3
    }
  }

  implicit val expressionDecompiler = new Decompile[ContentType] {
    override def key = "CONTENT_TYPE"
    override def decompileValue(value: ContentType): String = value match {
      case Expression => "EXPRESSION"
      case DApp       => "DAPP"
    }
  }

  implicit val scriptDecompiler = new Decompile[ScriptType] {
    override def key = "SCRIPT_TYPE"
    override def decompileValue(value: ScriptType): String = value match {
      case Account => "ACCOUNT"
      case Asset   => "ASSET"
    }
  }
}
