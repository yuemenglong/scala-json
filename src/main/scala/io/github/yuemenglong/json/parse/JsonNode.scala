package io.github.yuemenglong.json.parse

import io.github.yuemenglong.json.JSON
import io.github.yuemenglong.json.kit.Kit

import scala.util.matching.Regex

/**
  * Created by Administrator on 2017/7/5.
  */

trait AsT {
  def as[T](clazz: Class[T]): T

  def asObj(): JsonObj

  def asArr(): JsonArr

  def asInt(): Integer

  def asStr(): String

  def asLong(): java.lang.Long

  def asDouble(): java.lang.Double

  def asBool(): java.lang.Boolean
}

abstract class JsonNode extends AsT {
  override def as[T](clazz: Class[T]): T = {
    Convert.fromNodeToValue(this, clazz).asInstanceOf[T]
  }

  override def asObj(): JsonObj = this.asInstanceOf[JsonObj]

  override def asArr(): JsonArr = this.asInstanceOf[JsonArr]

  override def asInt(): Integer = this.as(classOf[Integer])

  override def asStr(): String = this.as(classOf[String])

  override def asLong(): java.lang.Long = this.as(classOf[java.lang.Long])

  override def asDouble(): java.lang.Double = this.as(classOf[java.lang.Double])

  override def asBool(): java.lang.Boolean = this.as(classOf[java.lang.Boolean])

  override def toString: String = toString(false)

  def path(path: String): JsonNode = {
    val re = """(([^.\[\]]+)|(\[\d+]))""".r
    re.findAllMatchIn(path).foldLeft(this)((node: JsonNode, m: Regex.Match) => {
      val key = m.group(0)
      if (node == null || node.isInstanceOf[JsonNull]) {
        JsonNull()
      } else if (key.matches("\\[\\d+]")) {
        val idx = key.slice(1, key.length - 1).toInt
        if (node.isInstanceOf[JsonArr]) {
          if (idx >= node.asArr().array.length) {
            JsonNull()
          } else {
            node.asArr().array(idx)
          }
        } else {
          throw new RuntimeException(s"Not Array For Key: $key, ${node.toString}")
        }
      } else {
        if (node.isInstanceOf[JsonObj]) {
          if (node.asObj().map.contains(key)) {
            node.asObj().map(key)
          } else {
            JsonNull()
          }
        } else {
          throw new RuntimeException(s"Not Object For Key: $key, ${node.toString}")
        }
      }
    })
  }

  def toString(stringifyNull: Boolean): String

  def toJsString: String = toJsString(false)

  def toJsString(stringifyNull: Boolean): String = toString(stringifyNull)

  def pretty(indent: Int = 1, tab: Int = 2): String = toString
}

case class JsonNull() extends JsonNode {
  override def toString(stringifyNull: Boolean): String = "null"
}

case class JsonBool(var value: Boolean) extends JsonNode {
  override def toString(stringifyNull: Boolean): String = s"$value"
}

abstract class JsonValue extends JsonNode {
  def toInt: Int

  def toLong: Long

  def toDouble: Double

  def toBool: Boolean

  def toStr: String
}

case class JsonLong(var value: Long) extends JsonValue {
  override def toString(stringifyNull: Boolean): String = s"$value"

  override def toInt: Int = value.toInt

  override def toLong: Long = value

  override def toDouble: Double = value.toDouble

  override def toStr: String = toString

  override def toJsString(stringifyNull: Boolean): String = {
    if (value > Integer.MAX_VALUE) {
      s""""$value""""
    } else {
      toString(stringifyNull)
    }
  }

  override def toBool: Boolean = value match {
    case 0 => false
    case _ => true
  }
}

case class JsonDouble(var value: Double) extends JsonValue {
  override def toString(stringifyNull: Boolean): String = s"$value"

  override def toLong: Long = value.toLong

  override def toDouble: Double = value

  override def toStr: String = toString

  override def toInt: Int = value.toInt

  override def toBool: Boolean = value match {
    case 0 => false
    case _ => true
  }
}

case class JsonStr(var value: String) extends JsonValue {
  override def toString(stringifyNull: Boolean): String = s""""${Kit.escapeString(value)}""""

  override def toLong: Long = value.toLong

  override def toDouble: Double = value.toDouble

  override def toStr: String = value

  override def toInt: Int = value.toInt

  override def toBool: Boolean = value match {
    case "false" | "0" => false
    case "true" | "1" => true
    case _ => throw new RuntimeException("String To Boolean Must Be true/1 Or false/0")
  }
}

case class JsonObj(var map: Map[String, JsonNode], fields: Array[String] = Array()) extends JsonNode {
  private def sortedMap = {
    map.toArray.sortBy { case (f, _) => fields.indexOf(f) }
  }

  override def toString(stringifyNull: Boolean): String = {
    val content = sortedMap.filter(p => {
      stringifyNull || !p._2.isInstanceOf[JsonNull]
    }).map(p => {
      val (name, node) = p
      s""""${Kit.escapeString(name)}":${node.toString(stringifyNull)}"""
    }).mkString(",")
    s"{$content}"
  }

  override def pretty(indent: Int, tab: Int): String = {
    val space = " " * indent * tab
    val spaceEnd = " " * (indent - 1) * tab
    val content = sortedMap.map(p => {
      val (name, node) = p
      s""""${Kit.escapeString(name)}": ${node.pretty(indent + 1, tab)}"""
    }).mkString(s",\n$space")
    s"{\n$space$content\n$spaceEnd}"
  }

  override def toJsString(stringifyNull: Boolean): String = {
    val content = sortedMap.filter(p => {
      stringifyNull || !p._2.isInstanceOf[JsonNull]
    }).map(p => {
      val (name, node) = p
      s""""$name":${node.toJsString(stringifyNull)}"""
    }).mkString(",")
    s"{$content}"
  }

  def size(): Int = {
    map.size
  }

  def get(name: String): JsonNode = {
    map(name)
  }

  def set(name: String, value: JsonNode): Unit = {
    map += (name -> value)
  }

  def remove(name: String): Unit = {
    map -= name
  }

  def clear(): Unit = {
    map = Map[String, JsonNode]()
  }

  def contains(name: String): Boolean = {
    map.contains(name)
  }

  def getBool(name: String): java.lang.Boolean = {
    if (!contains(name) || map(name).isInstanceOf[JsonNull]) {
      return null
    }
    map(name).asInstanceOf[JsonBool].value
  }

  def getInt(name: String): java.lang.Integer = {
    if (!contains(name) || map(name).isInstanceOf[JsonNull]) {
      return null
    }
    map(name).asInstanceOf[JsonValue].toInt
  }

  def getLong(name: String): java.lang.Long = {
    if (!contains(name) || map(name).isInstanceOf[JsonNull]) {
      return null
    }
    map(name).asInstanceOf[JsonValue].toLong
  }

  def getDouble(name: String): java.lang.Double = {
    if (!contains(name) || map(name).isInstanceOf[JsonNull]) {
      return null
    }
    map(name).asInstanceOf[JsonValue].toDouble
  }

  def getStr(name: String): String = {
    if (!contains(name) || map(name).isInstanceOf[JsonNull]) {
      return null
    }
    map(name).asInstanceOf[JsonValue].toStr
  }

  def getObj(name: String): JsonObj = {
    if (!contains(name) || map(name).isInstanceOf[JsonNull]) {
      return null
    }
    map(name).asInstanceOf[JsonObj]
  }

  def getArr(name: String): JsonArr = {
    if (!contains(name) || map(name).isInstanceOf[JsonNull]) {
      return null
    }
    map(name).asInstanceOf[JsonArr]
  }

  def setBool(name: String, value: java.lang.Boolean): Unit = {
    if (value == null) {
      map += (name -> JsonNull())
    } else {
      map += (name -> JsonBool(value))
    }
  }

  def setInt(name: String, value: java.lang.Integer): Unit = {
    if (value == null) {
      map += (name -> JsonNull())
    } else {
      map += (name -> JsonLong(value.longValue()))
    }
  }

  def setLong(name: String, value: java.lang.Long): Unit = {
    if (value == null) {
      map += (name -> JsonNull())
    } else {
      map += (name -> JsonLong(value))
    }
  }

  def setDouble(name: String, value: java.lang.Double): Unit = {
    if (value == null) {
      map += (name -> JsonNull())
    } else {
      map += (name -> JsonDouble(value))
    }
  }

  def setStr(name: String, value: String): Unit = {
    if (value == null) {
      map += (name -> JsonNull())
    } else {
      map += (name -> JsonStr(value))
    }
  }

  def setPlain(name: String, value: String): Unit = {
    if (value == null) {
      map += (name -> JsonNull())
    } else {
      map += (name -> JSON.parse(value))
    }
  }
}

case class JsonArr(var array: Array[JsonNode]) extends JsonNode {
  override def toString(stringifyNull: Boolean): String = {
    val content = array.filter(node => {
      stringifyNull || !node.isInstanceOf[JsonNull]
    }).map(node => {
      s"${node.toString(stringifyNull)}"
    }).mkString(",")
    s"[$content]"
  }

  override def pretty(indent: Int, tab: Int): String = {
    val content = array.map(node => {
      s"${node.pretty(indent, tab)}"
    }).mkString(", ")
    s"[$content]"
  }

  override def toJsString(stringifyNull: Boolean): String = {
    val content = array.filter(node => {
      stringifyNull || !node.isInstanceOf[JsonNull]
    }).map(node => {
      s"${node.toJsString(stringifyNull)}"
    }).mkString(",")
    s"[$content]"
  }

  def length(): Int = {
    array.length
  }

  def clear(): Unit = {
    array = Array()
  }

  def push(node: JsonNode): Unit = {
    array = array ++ Array(node)
  }

  def pushBool(name: String, value: java.lang.Boolean): Unit = {
    if (value == null) {
      array ++= Array(JsonNull())
    } else {
      array ++= Array(JsonBool(value))
    }
  }

  def pushInt(name: String, value: java.lang.Integer): Unit = {
    if (value == null) {
      array ++= Array(JsonNull())
    } else {
      array ++= Array(JsonLong(value.longValue()))
    }
  }

  def pushLong(name: String, value: java.lang.Long): Unit = {
    if (value == null) {
      array ++= Array(JsonNull())
    } else {
      array ++= Array(JsonLong(value))
    }
  }

  def pushDouble(name: String, value: java.lang.Double): Unit = {
    if (value == null) {
      array ++= Array(JsonNull())
    } else {
      array ++= Array(JsonDouble(value))
    }
  }

  def pushStr(name: String, value: String): Unit = {
    if (value == null) {
      array ++= Array(JsonNull())
    } else {
      array ++= Array(JsonStr(value))
    }
  }

  def pushPlain(name: String, value: String): Unit = {
    if (value == null) {
      array ++= Array(JsonNull())
    } else {
      array ++= Array(JSON.parse(value))
    }
  }

  def get(idx: Int): JsonNode = {
    array(idx)
  }

  def getBool(idx: Int): java.lang.Boolean = {
    if (array.length <= idx || array(idx).isInstanceOf[JsonNull]) {
      return null
    }
    array(idx).asInstanceOf[JsonBool].value
  }

  def getInt(idx: Int): java.lang.Integer = {
    if (array.length <= idx || array(idx).isInstanceOf[JsonNull]) {
      return null
    }
    array(idx).asInstanceOf[JsonValue].toInt
  }

  def getLong(idx: Int): java.lang.Long = {
    if (array.length <= idx || array(idx).isInstanceOf[JsonNull]) {
      return null
    }
    array(idx).asInstanceOf[JsonValue].toLong
  }

  def getDouble(idx: Int): java.lang.Double = {
    if (array.length <= idx || array(idx).isInstanceOf[JsonNull]) {
      return null
    }
    array(idx).asInstanceOf[JsonValue].toDouble
  }

  def getStr(idx: Int): String = {
    if (array.length <= idx || array(idx).isInstanceOf[JsonNull]) {
      return null
    }
    array(idx).asInstanceOf[JsonValue].toStr
  }

  def getObj(idx: Int): JsonObj = {
    if (array.length <= idx || array(idx).isInstanceOf[JsonNull]) {
      return null
    }
    array(idx).asInstanceOf[JsonObj]
  }

  def getArr(idx: Int): JsonArr = {
    if (array.length <= idx || array(idx).isInstanceOf[JsonNull]) {
      return null
    }
    array(idx).asInstanceOf[JsonArr]
  }
}

case class JsonPlain(var value: String) extends JsonNode {
  override def toString(stringifyNull: Boolean): String = {
    value
  }
}