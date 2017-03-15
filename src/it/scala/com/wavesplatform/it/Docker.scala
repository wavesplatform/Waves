package com.wavesplatform.it

import java.util.{Collections, Properties, Map => JMap, List => JList}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.javaprop.JavaPropsMapper
import com.google.common.collect.ImmutableMap
import com.spotify.docker.client.DefaultDockerClient
import com.spotify.docker.client.messages.{ContainerConfig, HostConfig, PortBinding}
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}

import scala.collection.JavaConverters._

case class NodeInfo(
    hostRestApiPort: Int,
    hostNetworkPort: Int,
    containerNetworkPort: Int,
    ipAddress: String,
    containerId: String)

trait Docker extends AutoCloseable {
  def startNode(config: Config = ConfigFactory.empty()): NodeInfo
  def stopNode(containerId: String)
}

object Docker {

  private val jsonMapper = new ObjectMapper
  private val propsMapper = new JavaPropsMapper
  private val confTemplate = ConfigFactory.parseResources("template.conf")
  private val imageId = System.getProperty("docker.imageId")

  private def asProperties(config: Config): Properties = {
    val jsonConfig = config.root().render(ConfigRenderOptions.concise())
    propsMapper.writeValueAsProperties(jsonMapper.readTree(jsonConfig))
  }

  private def renderProperties(p: Properties) = p.asScala.map { case (k, v) => s"-D$k=$v" } mkString " "

  private def extractHostPort(m: JMap[String, JList[PortBinding]], containerPort: String) =
    m.get(s"$containerPort/tcp").get(0).hostPort().toInt

  def apply(): Docker = new Docker {
    private val client = DefaultDockerClient.fromEnv().build()
    private var nodes = Map.empty[String, NodeInfo]

    private def knownPeers = nodes.values.zipWithIndex.map {
      case (ni, index) => s"-Dwaves.network.known-peers.$index=${ni.ipAddress}:${ni.containerNetworkPort}"
    } mkString " "

    override def startNode(config: Config): NodeInfo = {
      val configOverrides = s"$knownPeers ${renderProperties(asProperties(config))}"
      val actualConfig = config.withFallback(confTemplate)
      val restApiPort = actualConfig.getString("waves.rest-api.port")
      val networkPort = actualConfig.getString("waves.network.port")

      val portBindings = new ImmutableMap.Builder[String, java.util.List[PortBinding]]()
        .put(restApiPort, Collections.singletonList(PortBinding.randomPort("0.0.0.0")))
        .put(networkPort, Collections.singletonList(PortBinding.randomPort("0.0.0.0")))
        .build()

      val hostConfig = HostConfig.builder()
        .portBindings(portBindings)
        .build()

      val containerConfig = ContainerConfig.builder()
        .image(imageId)
        .exposedPorts(restApiPort, networkPort)
        .hostConfig(hostConfig)
        .env(s"""WAVES_OPTS=$configOverrides""")
        .build()

      val containerId = client.createContainer(containerConfig).id()
      client.startContainer(containerId)
      val containerInfo = client.inspectContainer(containerId)

      val ports = containerInfo.networkSettings().ports()

      val nodeInfo = NodeInfo(
        extractHostPort(ports, restApiPort),
        extractHostPort(ports, networkPort),
        networkPort.toInt,
        containerInfo.networkSettings().ipAddress(),
        containerId)
      nodes += containerId -> nodeInfo

      nodeInfo
    }

    override def stopNode(containerId: String): Unit = {
//      client.stopContainer(containerId, 10)
    }

    override def close(): Unit = {
//      nodes.keys.foreach(client.stopContainer(_, 10))
//      nodes.keys.foreach(client.removeContainer)
      client.close()
    }
  }
}
