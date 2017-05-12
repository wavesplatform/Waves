package com.wavesplatform

import java.net.{InetSocketAddress, URI}

package object network {
  def inetSocketAddress(addr: String, defaultPort: Int): InetSocketAddress = {
    val uri = new URI(s"my://$addr")
    if (uri.getPort < 0) new InetSocketAddress(addr, defaultPort)
    else new InetSocketAddress(uri.getHost, uri.getPort)
  }
}
