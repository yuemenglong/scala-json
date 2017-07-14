package yy.json.parse

/**
  * Created by Administrator on 2017/7/5.
  */
abstract class JsonNode extends AsT {
  override def as[T](clazz: Class[T]): T = {
    (this, clazz.isArray) match {
      case (arr: JsonArr, true) => Convert.toArray(arr, clazz).asInstanceOf[T]
      case (obj: JsonObj, false) => Convert.toObject(obj, clazz).asInstanceOf[T]
      case _ => throw new RuntimeException("Type Not Match, Maybe Need Pass Array Class ")
    }
  }

  def toString(stringifyNull: Boolean): String

  override def toString: String = toString(false)
}

case class JsonNull() extends JsonNode {
  override def toString(stringifyNull: Boolean): String = {
    "null"
  }
}

case class JsonBool(var value: Boolean) extends JsonNode {
  override def toString(stringifyNull: Boolean): String = {
    s"$value"
  }
}

//case class JsonNum(var value: Double) extends JsonNode {
//  override def toString(stringifyNull: Boolean): String = {
//    s"$value"
//  }
//}

abstract class JsonValue extends JsonNode {
  def toLong: Long

  def toDouble: Double

  def toStr: String
}


case class JsonLong(var value: Long) extends JsonValue {
  override def toString(stringifyNull: Boolean): String = s"$value"

  override def toLong: Long = value

  override def toDouble: Double = value.toDouble

  override def toStr: String = toString
}

case class JsonDouble(var value: Double) extends JsonValue {
  override def toString(stringifyNull: Boolean): String = s"$value"

  override def toLong: Long = value.toLong

  override def toDouble: Double = value

  override def toStr: String = toString
}

case class JsonStr(var value: String) extends JsonValue {
  override def toString(stringifyNull: Boolean): String = s""""$value""""

  override def toLong: Long = value.toLong

  override def toDouble: Double = value.toDouble

  override def toStr: String = value
}

case class JsonObj(var map: Map[String, JsonNode]) extends JsonNode {
  override def toString(stringifyNull: Boolean): String = {
    val content = map.toList.filter(p => {
      stringifyNull || !p._2.isInstanceOf[JsonNull]
    }).map(p => {
      val (name, node) = p
      s""""$name": ${node.toString(stringifyNull)}"""
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

  def contains(name: String): Boolean = {
    map.contains(name)
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

  def length(): Int = {
    array.length
  }

  def push(node: JsonNode): Unit = {
    array = array ++ Array(node)
  }

  def get(idx: Int): JsonNode = {
    array(idx)
  }
}

case class JsonPlain(var value: String) extends JsonNode {
  override def toString(stringifyNull: Boolean): String = {
    value
  }
}