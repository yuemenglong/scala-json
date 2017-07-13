package yy.json.parse

import java.lang
import java.lang.reflect.{Field, Method}

import yy.json.kit.Kit

import scala.reflect.ClassTag

trait AsT {
  def as[T](clazz: Class[T]): T
}

/**
  * Created by Administrator on 2017/7/13.
  */
object Convert {
  val classOfInteger: Class[Integer] = classOf[lang.Integer]
  val classOfLong: Class[lang.Long] = classOf[lang.Long]
  val classOfDouble: Class[lang.Double] = classOf[lang.Double]
  val classOfBoolean: Class[lang.Boolean] = classOf[lang.Boolean]
  val classOfString: Class[lang.String] = classOf[lang.String]

  var constructorMap: Map[String, () => Any] = Map[String, () => Any]()

  def setConstructorMap(map: Map[String, () => Any]): Unit = {
    constructorMap = map
  }

  def toNumber(n: JsonNum, clazz: Class[_]): Object = {
    clazz match {
      case `classOfInteger` => new lang.Integer(n.value.toInt)
      case `classOfLong` => new lang.Long(n.value.toLong)
      case `classOfDouble` => new lang.Double(n.value)
    }
  }

  def fromNumber(value: Object): JsonNum = {
    value.getClass match {
      case `classOfInteger` => JsonNum(value.asInstanceOf[lang.Integer].doubleValue())
      case `classOfLong` => JsonNum(value.asInstanceOf[lang.Long].doubleValue())
      case `classOfDouble` => JsonNum(value.asInstanceOf[lang.Double].doubleValue())
    }
  }

  def toArray(arr: JsonArr, clazz: Class[_]): Array[Object] = {
    val itemClazz = Kit.getArrayType(clazz)
    val ct = ClassTag(itemClazz).asInstanceOf[ClassTag[Object]]
    arr.array.map(fromNodeToValue(_, itemClazz)).toArray(ct)
  }

  def toObject(obj: JsonObj, clazz: Class[_]): Object = {
    val fields: Map[String, Field] = Kit.getDeclaredFields(clazz)
      .map(f => (f.getName, f))(collection.breakOut)
    val methods: Map[String, Method] = Kit.getDeclaredMethod(clazz)
      .map(m => (m.getName, m))(collection.breakOut)
    val ret = if (constructorMap.contains(clazz.getSimpleName)) {
      constructorMap(clazz.getSimpleName)()
    } else {
      clazz.newInstance()
    }
    obj.map.toArray.filter(p => fields.contains(p._1) && methods.contains(s"set${Kit.upperCaseFirst(p._1)}"))
      .foreach { case (name, node) =>
        val field = fields(name)
        val value = fromNodeToValue(node, field.getType)
        val method = methods(s"set${Kit.upperCaseFirst(name)}")
        method.invoke(ret, value.asInstanceOf[Object])
      }
    ret.asInstanceOf[Object]
  }

  def fromObject(obj: Object): JsonObj = {
    val fields: Array[Field] = Kit.getDeclaredFields(obj.getClass)
    val methods: Map[String, Method] = Kit.getDeclaredMethod(obj.getClass)
      .map(m => (m.getName, m))(collection.breakOut)
    val map: Map[String, JsonNode] = fields.filter(f => methods.contains(s"get${Kit.upperCaseFirst(f.getName)}"))
      .map(f => {
        val method = methods(s"get${Kit.upperCaseFirst(f.getName)}")
        val value = method.invoke(obj)
        val name = f.getName
        val node = fromValueToNode(value)
        (name, node)
      })(collection.breakOut)
    JsonObj(map)
  }

  def fromArray(value: Object): JsonNode = {
    val arr = value.asInstanceOf[Array[Object]]
    JsonArr(arr.map(fromValueToNode))
  }

  def fromValueToNode(value: Object): JsonNode = {
    if (value == null) {
      return JsonNull()
    }
    value.getClass match {
      case `classOfInteger` | `classOfLong` | `classOfDouble` => fromNumber(value)
      case `classOfString` => JsonStr(value.asInstanceOf[lang.String])
      case `classOfBoolean` => JsonBool(value.asInstanceOf[lang.Boolean])
      case t => if (t.isArray) {
        fromArray(value)
      } else {
        fromObject(value)
      }
    }
  }

  def fromNodeToValue(node: JsonNode, clazz: Class[_]): Object = {
    node match {
      case o: JsonObj => toObject(o, clazz)
      case a: JsonArr => toArray(a, clazz)
      case _: JsonNull => null
      case n: JsonNum => toNumber(n, clazz)
      case b: JsonBool => new lang.Boolean(b.value)
      case s: JsonStr => s.value
    }
  }
}
