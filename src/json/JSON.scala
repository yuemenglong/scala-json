package json

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


}
