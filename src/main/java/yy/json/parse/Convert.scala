package yy.json.parse

import java.lang
import java.lang.reflect.{Field, Method}

import yy.json.kit.Kit

import scala.reflect.ClassTag


/**
  * Created by Administrator on 2017/7/13.
  */
object Convert {
  val classOfJavaInteger: Class[Integer] = classOf[lang.Integer]
  val classOfJavaLong: Class[lang.Long] = classOf[lang.Long]
  val classOfJavaDouble: Class[lang.Double] = classOf[lang.Double]
  val classOfJavaBoolean: Class[lang.Boolean] = classOf[lang.Boolean]
  val classOfJavaString: Class[lang.String] = classOf[lang.String]

  val classOfScalaInt: Class[Int] = classOf[Int]
  val classOfScalaLong: Class[Long] = classOf[Long]
  val classOfScalaDouble: Class[Double] = classOf[Double]
  val classOfScalaBoolean: Class[Boolean] = classOf[Boolean]
  val classOfScalaString: Class[String] = classOf[String]

  var constructorMap: Map[Class[_], () => Any] = Map[Class[_], () => Any]()

  def setConstructorMap(map: Map[Class[_], () => Any]): Unit = {
    constructorMap = map
  }

  def toNumber(n: JsonValue, clazz: Class[_]): Object = {
    clazz match {
      case `classOfJavaInteger` | `classOfScalaInt` => new lang.Integer(n.toInt)
      case `classOfJavaLong` | `classOfScalaLong` => new lang.Long(n.toLong)
      case `classOfJavaDouble` | `classOfScalaDouble` => new lang.Double(n.toDouble)
    }
  }

  def fromNumber(value: Object): JsonValue = {
    value.getClass match {
      case `classOfJavaInteger` | `classOfScalaInt` => JsonLong(value.asInstanceOf[lang.Integer].longValue())
      case `classOfJavaLong` | `classOfScalaLong` => JsonLong(value.asInstanceOf[lang.Long].longValue())
      case `classOfJavaDouble` | `classOfScalaDouble` => JsonDouble(value.asInstanceOf[lang.Double].doubleValue())
    }
  }

  def toArray(arr: JsonArr, clazz: Class[_]): Array[Object] = {
    val itemClazz = Kit.getArrayType(clazz)
    val ct = ClassTag(itemClazz).asInstanceOf[ClassTag[Object]]
    arr.array.map(fromNodeToValue(_, itemClazz)).toArray(ct)
  }

  def toObject(obj: JsonObj, clazz: Class[_]): Object = {
    def setMethodNameJ(fieldName: String) = s"set${Kit.upperCaseFirst(fieldName)}"

    def setMethodNameS(fieldName: String) = s"${fieldName}_$$eq"

    val fields: Map[String, Field] = Kit.getDeclaredFields(clazz)
      .map(f => (f.getName, f))(collection.breakOut)
    val methods: Map[String, Method] = Kit.getDeclaredMethod(clazz)
      .map(m => (m.getName, m))(collection.breakOut)
    val ret = if (constructorMap.contains(clazz)) {
      constructorMap(clazz)()
    } else {
      clazz.newInstance()
    }
    obj.map.toArray.filter(p => fields.contains(p._1))
      .foreach { case (name, node) =>
        val field = fields(name)
        val value = fromNodeToValue(node, field.getType)
        val method = if (methods.contains(setMethodNameJ(name))) {
          methods(setMethodNameJ(name))
        } else if (methods.contains(setMethodNameS(name))) {
          methods(setMethodNameS(name))
        } else {
          throw new RuntimeException(s"No Setter Found Of $name")
        }
        method.invoke(ret, value.asInstanceOf[Object])
      }
    ret.asInstanceOf[Object]
  }

  def fromObject(obj: Object): JsonObj = {
    def getMethodNameJ(fieldName: String) = s"get${Kit.upperCaseFirst(fieldName)}"

    def getMethodNameS(fieldName: String) = fieldName

    val fields: Array[Field] = Kit.getDeclaredFields(obj.getClass)
    val methods: Map[String, Method] = Kit.getDeclaredMethod(obj.getClass)
      .map(m => (m.getName, m))(collection.breakOut)
    val map: Map[String, JsonNode] = fields
      .filter(f => methods.contains(getMethodNameJ(f.getName)) || methods.contains(f.getName))
      .map(f => {
        val name = f.getName
        val value = if (methods.contains(getMethodNameJ(name))) {
          methods(getMethodNameJ(name)).invoke(obj)
        } else if (methods.contains(getMethodNameS(name))) {
          methods(getMethodNameS(name)).invoke(obj)
        } else {
          throw new RuntimeException(s"No Getter On $name")
        }
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
      case `classOfJavaInteger` | `classOfJavaLong` | `classOfJavaDouble`
           | `classOfScalaInt` | `classOfScalaLong` | `classOfScalaDouble` => fromNumber(value)
      case `classOfJavaString` | `classOfScalaString` => JsonStr(value.asInstanceOf[lang.String])
      case `classOfJavaBoolean` | `classOfScalaBoolean` => JsonBool(value.asInstanceOf[lang.Boolean])
      case t => if (t.isArray) {
        fromArray(value)
      } else {
        fromObject(value)
      }
    }
  }

  def fromNodeToValue(node: JsonNode, clazz: Class[_]): Object = {
    if (node.isInstanceOf[JsonNull]) {
      return null
    }
    (clazz, node) match {
      case (`classOfJavaString` | `classOfScalaString`, v: JsonValue) => v.toStr
      case (`classOfJavaInteger` | `classOfScalaInt`, v: JsonValue) => new lang.Integer(v.toInt)
      case (`classOfJavaLong` | `classOfScalaLong`, v: JsonValue) => new lang.Long(v.toLong)
      case (`classOfJavaDouble` | `classOfScalaDouble`, v: JsonValue) => new lang.Double(v.toDouble)
      case (`classOfJavaBoolean` | `classOfScalaBoolean`, v: JsonBool) => new lang.Boolean(v.value)
      case (_, v: JsonObj) => toObject(v, clazz)
      case (_, v: JsonArr) => toArray(v, clazz)
      case _ => throw new RuntimeException("Clazz And Node Type Not Match")
    }
  }
}
