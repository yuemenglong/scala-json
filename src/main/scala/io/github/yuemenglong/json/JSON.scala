package io.github.yuemenglong.json

import io.github.yuemenglong.json.kit.Kit
import io.github.yuemenglong.json.parse._

/**
  * Created by Administrator on 2017/7/5.
  */
object JSON {

  object types {
    type Integer = java.lang.Integer
    type Long = java.lang.Long
    type Double = java.lang.Double
    type Boolean = java.lang.Boolean
  }

  def parse(json: String): JsonNode = {
    val str = json.trim
    val (node, length) = (str(0), str.last) match {
      case ('{', '}') => Parse.parseObj(str, 0)
      case ('[', ']') => Parse.parseArr(str, 0)
      case _ => throw new RuntimeException("Invalid Json Format")
    }
    if (length != str.length - 1) {
      throw new RuntimeException("Json End Before String End")
    }
    node
  }

  def parse[T](json: String, clazz: Class[T]): T = parse(json).as(clazz)

  def parse[T](clazz: Class[T], json: String): T = parse(json, clazz)

  def convert(obj: Object): JsonNode = {
    Convert.fromValueToNode(obj)
  }

  def convert[T](node: JsonNode, clazz: Class[T]): T = {
    node.as(clazz)
  }

  def stringify(obj: Object): String = convert(obj).toString

  def stringify(obj: Object, stringifyNull: Boolean): String = convert(obj).toString(stringifyNull)

  def stringifyJs(obj: Object): String = convert(obj).toJsString

  def stringifyJs(obj: Object, stringifyNull: Boolean): String = convert(obj).toJsString(stringifyNull)

  def pretty(obj: Object): String = convert(obj).pretty()

  def walk(node: JsonNode, fn: JsonNode => JsonNode): JsonNode = {
    Walk.walk(node, fn)
  }

  def obj(): JsonObj = JsonObj(Map())

  def obj(map: Map[String, _]): JsonObj = {
    JsonObj(map.mapValues(o => Convert.fromValueToNode(o.asInstanceOf[Object])))
  }

  def arr(): JsonArr = JsonArr(Array())

  def arr(arr: Array[_]): JsonArr = {
    JsonArr(arr.map(o => Convert.fromValueToNode(o.asInstanceOf[Object])))
  }

  def int(i: Int): JsonLong = JsonLong(i)

  def long(l: Long): JsonLong = JsonLong(l)

  def double(d: Double): JsonDouble = JsonDouble(d)

  def bool(b: Boolean): JsonBool = JsonBool(b)

  def setConstructorMap(map: Map[Class[_], () => Object]): Unit = Convert.setConstructorMap(map)

  def setDebug(flag: Boolean): Unit = {
    Kit.debugFlag = flag
  }
}
