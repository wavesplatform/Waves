package com.wavesplatform

import shapeless.=:!=

package object meta {
  def getSimpleName[T](x: T)(implicit ev: T =:!= Class[?]): String = getSimpleClassName(x.getClass)

  def getSimpleClassName(x: Class[?]) = x.getName.replaceAll(".*?(\\w+)\\$?$", "$1")
}
