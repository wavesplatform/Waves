package com.wavesplatform.lang.v1

import com.wavesplatform.lang.v1.compiler.Errors.CompilationError
import com.wavesplatform.lang.v1.task.TaskM

package object compiler {
  type CompileM[A]      = TaskM[CompilerContext, CompilationError, A]
}
