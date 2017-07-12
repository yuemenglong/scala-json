package json

import json.parse.{JsonNode, Parser}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Administrator on 2017/7/5.
  */


object JSON {

  def constructorMap = Map[String, () => Any]()

  //  def parse[T](json: String, clazz: Class[_]): T = {
  //    val isArray = clazz.isAssignableFrom(classOf[Array[_]])
  //    (json(0), json.last, isArray) match {
  //      case ('{', '}', false) => parseObject[T](json)
  //      case ('[', ']', true) => parseArray[T](json)
  //      case _ => throw new RuntimeException("Brace And Type Not Match")
  //    }
  //  }

  def parse[T](json: String, obj: T): Unit = {

  }

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
}