package io.github.yuemenglong.json.kit

import java.lang.reflect.{Field, Method, ParameterizedType}
import java.util.concurrent.ConcurrentHashMap

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by Administrator on 2017/7/5.
  */
object Kit {

  var debugFlag = false

  def upperCaseFirst(str: String): String = {
    if (str == null) {
      return null
    }
    if (str.isEmpty) {
      return str
    }
    str(0).toUpper + str.substring(1)
  }

  def getArrayType(clazz: Class[_]): Class[_] = {
    if (!clazz.isArray) {
      return clazz
    }
    val name = clazz.getName.replaceAll("(^\\[L)|(;$)", "")
    Class.forName(name)
  }

  def isGenericType(clazz: Class[_]): Boolean = {
    clazz.getGenericSuperclass.isInstanceOf[ParameterizedType]
  }

  def getGenericType(clazz: Class[_]): Class[_] = {
    //返回表示此 Class 所表示的实体（类、接口、基本类型或 void）的直接超类的 Type。
    val genericType = clazz.getGenericSuperclass
    if (!genericType.isInstanceOf[ParameterizedType]) return clazz
    //返回表示此类型实际类型参数的 Type 对象的数组。
    val params = genericType.asInstanceOf[ParameterizedType].getActualTypeArguments
    params(0).asInstanceOf[Class[_]]
  }

  private val fieldCache: ConcurrentHashMap[Class[_], Array[Field]] = new ConcurrentHashMap[Class[_], Array[Field]]()
  private val methodCache: ConcurrentHashMap[Class[_], Array[Method]] = new ConcurrentHashMap[Class[_], Array[Method]]()

  def getDeclaredFields(clazz: Class[_]): Array[Field] = {
    if (fieldCache.contains(clazz)) {
      return fieldCache.get(clazz)
    }
    val ret = new ArrayBuffer[Field]()
    clazz.getDeclaredFields.foreach(ret += _)
    var parent = clazz.getSuperclass
    while (parent != null) {
      parent.getDeclaredFields.foreach(ret += _)
      parent = parent.getSuperclass
    }
    fieldCache.put(clazz, ret.toArray)
    fieldCache.get(clazz)
  }

  def getDeclaredMethod(clazz: Class[_]): Array[Method] = {
    if (methodCache.contains(clazz)) {
      return methodCache.get(clazz)
    }
    val ret = new ArrayBuffer[Method]()
    clazz.getDeclaredMethods.foreach(ret += _)
    var parent = clazz.getSuperclass
    while (parent != null) {
      parent.getDeclaredMethods.foreach(ret += _)
      parent = parent.getSuperclass
    }
    methodCache.put(clazz, ret.toArray)
    methodCache.get(clazz)
  }

  def escapeString(str: String): String = {
    """["\\\r\n\t]""".r.replaceAllIn(str, word => {
      word.group(0) match {
        case "\"" => "\\\\\""
        case "\\" => "\\\\\\\\"
        case "\r" => "\\\\r"
        case "\n" => "\\\\n"
        case "\t" => "\\\\t"
      }
    })
  }

  def unescapeString(c: Char): Char = {
    c match {
      case '"' => '"'
      case '\\' => '\\'
      case 'r' => '\r'
      case 'n' => '\n'
      case 't' => '\t'
    }
  }

  def debug(msg: String): Unit = {
    if (debugFlag) {
      println(msg)
    }
  }
}
