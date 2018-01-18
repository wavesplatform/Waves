package com.wavesplatform.it

import java.io.FileOutputStream
import java.nio.file.{Files, Paths}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean
import java.util.{Collections, Properties, List => JList, Map => JMap}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.javaprop.JavaPropsMapper
import com.google.common.collect.ImmutableMap
import com.spotify.docker.client.messages.EndpointConfig.EndpointIpamConfig
import com.spotify.docker.client.messages._
import com.spotify.docker.client.{DefaultDockerClient, DockerClient}
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.api.Node
import com.wavesplatform.it.util.GlobalTimer.{instance => timer}
import org.asynchttpclient.Dsl._
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, blocking}
import scala.util.Random
import scala.util.control.NonFatal

case class NodeInfo(restApi: String,
                    matcherApi: String,
                    hostNetworkPort: Int,
                    containerNetworkPort: Int,
                    networkIpAddress: String,
                    containerId: String,
                    apiKey: String)

class Docker(suiteConfig: Config = ConfigFactory.empty,
             tag: String = "") extends AutoCloseable with ScorexLogging {

  import Docker._

  private val http = asyncHttpClient(config()
    .setNettyTimer(timer)
    .setMaxConnections(18)
    .setMaxConnectionsPerHost(3)
    .setMaxRequestRetry(1)
    .setReadTimeout(10000)
    .setKeepAlive(false)
    .setRequestTimeout(10000))

  private val client = DefaultDockerClient.fromEnv().build()

  private val nodes = ConcurrentHashMap.newKeySet[NodeImpl]()
  private val isStopped = new AtomicBoolean(false)

  dumpContainers(client.listContainers())
  sys.addShutdownHook {
    log.debug("Shutdown hook")
    close()
  }

  private val networkPrefix = s"172.${125 + Random.nextInt(75)}.${1 + Random.nextInt(125)}"

  private lazy val wavesNetwork: Network = {
    val networkName = s"waves-${hashCode().toLong.toHexString}"

    def network: Option[Network] = try {
      val networks = client.listNetworks(DockerClient.ListNetworksParam.byNetworkName(networkName))
      if (networks.isEmpty) None else Some(networks.get(0))
    } catch {
      case NonFatal(_) => network
    }

    def attempt(rest: Int): Network = try {
      network match {
        case Some(n) =>
          val ipam = s"ipam: ${n.ipam().config().asScala.map { n => s"subnet=${n.subnet()}, ip range=${n.ipRange()}" }.mkString(", ")}"
          log.info(s"Network ${n.name()} (id: ${n.id()}) is created for $tag, $ipam")
          n
        case None =>
          log.debug(s"Creating network $networkName for $tag")
          // Specify the network manually because of race conditions: https://github.com/moby/moby/issues/20648
          val r = client.createNetwork(NetworkConfig.builder()
            .name(networkName)
            .ipam(
              Ipam.builder()
                .driver("default")
                .config(List(IpamConfig.create(s"$networkPrefix.0/24", s"$networkPrefix.0/24", s"$networkPrefix.254")).asJava)
                .build()
            )
            .checkDuplicate(true)
            .build())
          Option(r.warnings()).foreach(log.warn(_))
          attempt(rest - 1)
      }
    } catch {
      case NonFatal(e) =>
        log.warn(s"Can not create a network for $tag", e)
        if (rest == 0) throw e else attempt(rest - 1)
    }

    attempt(5)
  }

  def startNodes(nodeConfigs: Seq[Config]): Seq[Node] = {
    log.trace(s"Starting ${nodeConfigs.size} containers")
    val all = nodeConfigs.map(startNodeInternal)
    Await.result(
      for {
        _ <- Future.traverse(all)(_.waitForStartup())
        _ <- Future.traverse(all)(connectToAll)
      } yield (),
      5.minutes
    )
    all
  }

  def startNode(nodeConfig: Config, autoConnect: Boolean = true): Node = {
    val node = startNodeInternal(nodeConfig)
    Await.result(
      node.waitForStartup().flatMap(_ => if (autoConnect) connectToAll(node) else Future.successful(())),
      3.minutes
    )
    node
  }

  private def connectToAll(node: Node): Future[Unit] = {
    def connectToOne(host: String, port: Int): Future[Unit] = {
      for {
        _ <- node.connect(host, port)
        _ <- Future(blocking(Thread.sleep(3.seconds.toMillis)))
        connectedPeers <- node.connectedPeers
        _ <- {
          val connectedAddresses = connectedPeers.map(_.address.replaceAll("""^.*/([\d\.]+).+$""", "$1")).sorted
          log.debug(s"See for $host in $connectedAddresses")
          if (connectedAddresses.contains(host)) Future.successful(())
          else {
            log.debug(s"Not found $host, retry")
            connectToOne(host, port)
          }
        }
      } yield ()
    }

    val seedAddresses = nodes.asScala
      .filterNot(_.name == node.name)
      .map { n => (n.nodeInfo.networkIpAddress, n.nodeInfo.containerNetworkPort) }

    if (seedAddresses.isEmpty) Future.successful(())
    else Future
      .traverse(seedAddresses)(Function.tupled(connectToOne))
      .map(_ => ())
  }

  private def startNodeInternal(nodeConfig: Config): Node = try {
    val javaOptions = Option(System.getenv("CONTAINER_JAVA_OPTS")).getOrElse("")
    val configOverrides = s"$javaOptions ${renderProperties(asProperties(nodeConfig.withFallback(suiteConfig)))} " +
      s"-Dlogback.stdout.level=TRACE -Dlogback.file.level=OFF"

    val actualConfig = nodeConfig
      .withFallback(suiteConfig)
      .withFallback(NodeConfigs.DefaultConfigTemplate)
      .withFallback(ConfigFactory.defaultApplication())
      .withFallback(ConfigFactory.defaultReference())
      .resolve()

    val restApiPort = actualConfig.getString("waves.rest-api.port")
    val networkPort = actualConfig.getString("waves.network.port")
    val matcherApiPort = actualConfig.getString("waves.matcher.port")

    val portBindings = new ImmutableMap.Builder[String, java.util.List[PortBinding]]()
      .put(restApiPort, Collections.singletonList(PortBinding.randomPort("0.0.0.0")))
      .put(networkPort, Collections.singletonList(PortBinding.randomPort("0.0.0.0")))
      .put(matcherApiPort, Collections.singletonList(PortBinding.randomPort("0.0.0.0")))
      .build()

    val hostConfig = HostConfig.builder()
      .portBindings(portBindings)
      .build()

    val nodeName = actualConfig.getString("waves.network.node-name")
    val nodeNumber = nodeName.replace("node", "").toInt
    val ip = s"$networkPrefix.$nodeNumber"
    val containerConfig = ContainerConfig.builder()
      .image("com.wavesplatform/it:latest")
      .exposedPorts(restApiPort, networkPort, matcherApiPort)
      .networkingConfig(ContainerConfig.NetworkingConfig.create(Map(
        wavesNetwork.name() -> endpointConfigFor(nodeName)
      ).asJava))
      .hostConfig(hostConfig)
      .env(s"WAVES_OPTS=$configOverrides", s"WAVES_NET_IP=$ip", s"WAVES_PORT=$networkPort")
      .build()

    val containerId = {
      val containerName = s"${wavesNetwork.name()}-$nodeName"
      dumpContainers(
        client.listContainers(DockerClient.ListContainersParam.filter("name", containerName)),
        "Containers with same name"
      )

      log.debug(s"Creating container $containerName at $ip with options: $javaOptions")
      val r = client.createContainer(containerConfig, containerName)
      Option(r.warnings().asScala).toSeq.flatten.foreach(log.warn(_))
      r.id()
    }

    client.startContainer(containerId)

    val node = new NodeImpl(actualConfig, getNodeInfo(containerId, actualConfig), http)
    nodes.add(node)
    log.debug(s"Started $containerId -> ${node.name}: ${node.nodeInfo}")
    node
  } catch {
    case NonFatal(e) =>
      log.error("Can't start a container", e)
      dumpContainers(client.listContainers())
      throw e
  }

  private def getNodeInfo(node: Node): NodeInfo = getNodeInfo(node.nodeInfo.containerId, node.config)

  private def getNodeInfo(containerId: String, config: Config): NodeInfo = {
    val restApiPort = config.getString("waves.rest-api.port")
    val networkPort = config.getString("waves.network.port")
    val matcherApiPort = config.getString("waves.matcher.port")

    val containerInfo = inspectContainer(containerId)
    val ports = containerInfo.networkSettings().ports()

    NodeInfo(
      s"http://localhost:${extractHostPort(ports, restApiPort)}",
      s"http://localhost:${extractHostPort(ports, matcherApiPort)}",
      extractHostPort(ports, networkPort),
      networkPort.toInt,
      containerInfo.networkSettings().ipAddress(),
      containerId,
      "integration-test-rest-api"

    )
  }

  private def inspectContainer(containerId: String): ContainerInfo = {
    val containerInfo = client.inspectContainer(containerId)
    if (containerInfo.networkSettings().networks().asScala.contains(wavesNetwork.name())) containerInfo
    else {
      log.debug(s"Container $containerId has not connected to the network ${wavesNetwork.name()} yet, retry")
      Thread.sleep(1000)
      inspectContainer(containerId)
    }
  }

  override def close(): Unit = {
    if (isStopped.compareAndSet(false, true)) {
      log.info("Stopping containers")

      nodes.asScala.foreach { node =>
        client.stopContainer(node.nodeInfo.containerId, 0)

        saveLog(node)
        val containerInfo = client.inspectContainer(node.nodeInfo.containerId)
        log.debug(
          s"""Container information for ${node.name}:
             |Exit code: ${containerInfo.state().exitCode()}
             |Error: ${containerInfo.state().error()}
             |Status: ${containerInfo.state().status()}
             |OOM killed: ${containerInfo.state().oomKilled()}""".stripMargin)

        try {
          client.removeContainer(node.nodeInfo.containerId)
        } catch {
          case NonFatal(e) => log.warn(s"Can't remove a container of ${node.settings.networkSettings.nodeName}", e)
        }
      }

      try {
        client.removeNetwork(wavesNetwork.id)
      } catch {
        case NonFatal(e) =>
          // https://github.com/moby/moby/issues/17217
          log.warn(s"Can not remove network ${wavesNetwork.name()}", e)
      }

      http.close()
      client.close()
    }
  }

  private def saveLog(node: Node): Unit = {
    val logDir = Option(System.getProperty("waves.it.logging.dir")).map(Paths.get(_))
      .getOrElse(Paths.get(System.getProperty("user.dir"), "target", "logs"))

    Files.createDirectories(logDir)
    val containerId = node.nodeInfo.containerId

    val logFile = logDir.resolve(s"${node.name}.log").toFile
    log.info(s"Writing logs of $containerId to ${logFile.getAbsolutePath}")

    val fileStream = new FileOutputStream(logFile, false)
    try {
      client
        .logs(
          containerId,
          DockerClient.LogsParam.follow(),
          DockerClient.LogsParam.stdout(),
          DockerClient.LogsParam.stderr()
        )
        .attach(fileStream, fileStream)
    } finally {
      fileStream.close()
    }
  }

  def disconnectFromNetwork(node: Node): Unit = disconnectFromNetwork(node.nodeInfo.containerId)

  private def disconnectFromNetwork(containerId: String): Unit = client.disconnectFromNetwork(containerId, wavesNetwork.id())

  def connectToNetwork(nodes: Seq[Node]): Unit = {
    nodes.foreach(connectToNetwork)
    Await.result(Future.traverse(nodes)(connectToAll), 1.minute)
  }

  private def connectToNetwork(node: Node): Unit = {
    client.connectToNetwork(
      wavesNetwork.id(),
      NetworkConnection.builder()
        .containerId(node.nodeInfo.containerId)
        .endpointConfig(endpointConfigFor(node.name))
        .build()
    )

    node.nodeInfo = getNodeInfo(node)
    log.debug(s"New ${node.name} settings: ${node.nodeInfo}")
  }

  private def endpointConfigFor(nodeName: String): EndpointConfig = {
    val nodeNumber = nodeName.replace("node", "").toInt
    val ip = s"$networkPrefix.$nodeNumber"

    EndpointConfig.builder()
      .ipAddress(ip)
      .ipamConfig(EndpointIpamConfig.builder().ipv4Address(ip).build())
      .build()
  }

  private def dumpContainers(containers: java.util.List[Container], label: String = "Containers"): Unit = {
    val x = if (containers.isEmpty) "No" else "\n" + containers.asScala
      .map { x => s"Container(${x.id()}, status: ${x.status()}, names: ${x.names().asScala.mkString(", ")})" }
      .mkString("\n")

    log.debug(s"$label: $x")
  }
}

object Docker {
  private val jsonMapper = new ObjectMapper
  private val propsMapper = new JavaPropsMapper

  def apply(owner: Class[_]): Docker = new Docker(tag = owner.getSimpleName)

  private def asProperties(config: Config): Properties = {
    val jsonConfig = config.root().render(ConfigRenderOptions.concise())
    propsMapper.writeValueAsProperties(jsonMapper.readTree(jsonConfig))
  }

  private def renderProperties(p: Properties) = p.asScala.map { case (k, v) => s"-D$k=$v" } mkString " "

  private def extractHostPort(m: JMap[String, JList[PortBinding]], containerPort: String) =
    m.get(s"$containerPort/tcp").get(0).hostPort().toInt
}
