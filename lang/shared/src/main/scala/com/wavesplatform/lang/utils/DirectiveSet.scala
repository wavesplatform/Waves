package com.wavesplatform.lang.utils
import com.wavesplatform.lang.ContentType.ContentType
import com.wavesplatform.lang.ScriptType.ScriptType
import com.wavesplatform.lang.StdLibVersion.StdLibVersion

case class DirectiveSet(stdLibVersion: StdLibVersion, scriptType: ScriptType, contentType: ContentType)