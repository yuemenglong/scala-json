package yy.json

import yy.json.parse._

/**
  * Created by Administrator on 2017/7/5.
  */


object JSON {

  def parse(json: String): JsonNode = {
    val str = json.trim
    val (node, length) = (str(0), str.last) match {
      case ('{', '}') => Parser.parseObj(str, 0)
      case ('[', ']') => Parser.parseArr(str, 0)
      case _ => throw new RuntimeException("Invalid Json Format")
    }
    if (length != str.length - 1) {
      throw new RuntimeException("Json End Before String End")
    }
    node
  }

  def parse[T](json: String, clazz: Class[T]): T = parse(json).as(clazz)

  def convert(obj: Object): JsonNode = {
    if (obj.getClass.isArray) {
      Convert.fromArray(obj)
    } else {
      Convert.fromObject(obj)
    }
  }

  def stringify(obj: Object): String = convert(obj).toString
}
