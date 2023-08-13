package com.wavesplatform.json

import play.api.libs.json.{JsArray, JsObject, JsValue, Json}

import scala.annotation.tailrec

object JsonManipulations {
  def pruneAll(js: JsValue, paths: List[String]): JsObject =
    paths.foldLeft(JsObject.empty) { case (r, path) =>
      r.deepMerge(prune(js, path))
    }

  /** @param path
    *   dot-separated path, e.g.: "foo.bar.baz"
    * @return
    *   Removes all subtrees except a subtree with specified path
    */
  def prune(js: JsValue, path: String): JsObject = prune(js, path.split('.').toList)

  /** @return
    *   Removes all subtrees except a subtree with specified path
    */
  private def prune(js: JsValue, path: List[String]): JsObject =
    pick(js, path).fold(JsObject.empty) { leaf =>
      mkTrunk(path, leaf) match {
        case trunk: JsObject => trunk
        case _               => JsObject.empty // It's not a tree, just one leaf. Cut it!
      }
    }

  /** @param paths
    *   dot-separated paths, e.g.: "foo.bar.baz", "bar"
    * @return
    *   Subtrees by these paths in the same order
    */
  def pickAll(js: JsValue, paths: List[String]): JsValue = JsArray(
    paths
      .foldLeft(List.empty[JsValue]) { (r, path) =>
        pick(js, path) match {
          case None    => r
          case Some(x) => x :: r
        }
      }
      .reverse
  )

  /** @param path
    *   dot-separated path, e.g.: "foo.bar.baz"
    * @return
    *   A subtree by this path
    */
  def pick(js: JsValue, path: String): Option[JsValue] = pick(js, splitPath(path))

  /** @return
    *   A subtree by this path
    */
  private def pick(js: JsValue, path: List[String]): Option[JsValue] = {
    @tailrec def loop(js: JsValue, path: List[String]): Option[JsValue] = path match {
      case Nil => Some(js)
      case segment :: restSegments =>
        (js \ segment).asOpt[JsValue] match {
          case None          => None
          case Some(subTree) => loop(subTree, restSegments)
        }
    }

    if (path.isEmpty) None
    else loop(js, path)
  }

  /** @param path
    *   dot-separated path, e.g.: "foo.bar.baz"
    * @return
    *   A nested object with specified path and the deepest nested leaf
    */
  def mkTrunk(path: String, leaf: JsValue): JsValue = mkTrunk(splitPath(path), leaf)

  /** @return
    *   A nested object with specified path and the deepest nested leaf
    */
  private def mkTrunk(path: List[String], leaf: JsValue): JsValue = {
    @tailrec def loop(reversedPath: List[String], acc: JsValue): JsValue = reversedPath match {
      case innerSegment :: outerSegments => loop(outerSegments, Json.obj(innerSegment -> acc))
      case Nil                           => acc
    }

    if (path.isEmpty) leaf
    else loop(path.reverse, leaf)
  }

  private def splitPath(path: String): List[String] = {
    val trimmed = path.trim
    if (trimmed.isEmpty) Nil else trimmed.split('.').toList
  }
}
