package io.github.yuemenglong.json

import io.github.yuemenglong.json.kit.Kit
import io.github.yuemenglong.json.parse._

/**
  * Created by Administrator on 2017/7/5.
  */
object JSON {

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

  def convert(obj: Object): JsonNode = {
    Convert.fromValueToNode(obj)
    //    if (obj == null) {
    //      new JsonNull
    //    } else if (obj.getClass.isArray) {
    //      Convert.fromArray(obj)
    //    } else {
    //      Convert.fromObject(obj)
    //    }
  }

  def stringify(obj: Object): String = convert(obj).toString

  def stringify(obj: Object, stringifyNull: Boolean): String = convert(obj).toString(stringifyNull)

  def stringifyJs(obj: Object): String = convert(obj).toJsString

  def stringifyJs(obj: Object, stringifyNull: Boolean): String = convert(obj).toJsString(stringifyNull)

  def walk(node: JsonNode, fn: (JsonNode) => JsonNode): JsonNode = {
    Walk.walk(node, fn)
  }

  def obj(): JsonObj = JsonObj(Map())

  def arr(): JsonArr = JsonArr(Array())

  def setConstructorMap(map: Map[Class[_], () => Object]): Unit = Convert.setConstructorMap(map)

  def setDebug(flag: Boolean): Unit = {
    Kit.debugFlag = flag
  }
}
