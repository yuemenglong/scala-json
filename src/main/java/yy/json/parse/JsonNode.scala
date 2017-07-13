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

case class JsonNum(var value: Double) extends JsonNode {
  override def toString(stringifyNull: Boolean): String = {
    s"$value"
  }
}

case class JsonStr(var value: String) extends JsonNode {
  override def toString(stringifyNull: Boolean): String = {
    s""""$value""""
  }
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
}

case class JsonPlain(var value: String) extends JsonNode {
  override def toString(stringifyNull: Boolean): String = {
    value
  }
}