package com.wavesplatform.lang.directives.values
import com.wavesplatform.lang.directives.DirectiveDictionary

sealed abstract class ScriptType(text: String, id: Int) extends DirectiveValue(text, id) {
  override val value: Any = text
  override def key        = resolveKey[ScriptType]
}
case object Account extends ScriptType("ACCOUNT", 1)
case object Asset   extends ScriptType("ASSET", 2)

object ScriptType {
  implicit object ScriptDic extends DirectiveDictionary[ScriptType] {
    override val default: ScriptType           = Account
    override val all:     Iterable[ScriptType] = Seq(Account, Asset)
  }

  def isAssetScript(b: Boolean): ScriptType = if (b) Asset else Account
}
