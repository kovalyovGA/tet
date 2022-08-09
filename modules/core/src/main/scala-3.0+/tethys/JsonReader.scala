package tethys

import scala.deriving.Mirror
import scala.compiletime.summonInline
import tethys.derivation.utils.*
import tethys.derivation.builder.ReaderBuilder
import tethys.readers.instances.AllJsonReaders
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, JsonReaderBuilder, ReaderError}

import scala.collection.mutable.ArrayBuffer


trait JsonReader[@specialized(specializations) A] {
  self =>

  def read(it: TokenIterator)(implicit fieldName: FieldName): A

  def map[B](fun: A => B): JsonReader[B] = new JsonReader[B] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): B = fun(self.read(it))
  }

  def mapWithField[B](fun: FieldName => A => B): JsonReader[B] = new JsonReader[B] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): B = fun(fieldName)(self.read(it))
  }
}

object JsonReader extends AllJsonReaders:
  def apply[A](implicit jsonReader: JsonReader[A]): JsonReader[A] = jsonReader

  inline def derived[A](using mirror: Mirror.ProductOf[A]): JsonReader[A] =
    productReader(mirror)
      


  inline def macroReader[A]: JsonReader[A] =
    new JsonReader[A]:
      override def read(it: TokenIterator)(implicit fieldName: FieldName) =
        if(!it.currentToken().isObjectStart)
          ReaderError.wrongJson("Expected object start but found: "  + it.currentToken().toString)
        else
          it.nextToken()
          derivation.reader.read(ReaderBuilder[A], it, fieldName)



  private inline def productReader[A](mirror: Mirror.ProductOf[A]): JsonReader[A] =
    new JsonReader[A]:
      override def read(it: TokenIterator)(implicit fieldName: FieldName) =
        val labels = summonLabels[mirror.MirroredElemLabels].toArray
        val readers: Map[String, JsonReader[Any]] =
          labels.zip(summonAll[JsonReader, mirror.MirroredElemTypes]).toMap
        val uninitializedFields = collection.mutable.Set.from(labels)

        if(!it.currentToken().isObjectStart)
          ReaderError.wrongJson("Expected object start but found: "  + it.currentToken().toString)
        else
          it.nextToken()
          val values = collection.mutable.Map.empty[String, Any]
          val unknownKeys = ArrayBuffer.empty[String]
          while(!it.currentToken().isObjectEnd)
            val name = it.fieldName()
            readers.get(name) match
              case Some(reader) =>
                values += name -> reader.read(it.next())(fieldName.appendFieldName(name))
                uninitializedFields -= name
              case None =>
                unknownKeys += name
                it.next()
          if (uninitializedFields.isEmpty)
            val result = labels.map(values(_))
            mirror.fromProduct(buildProduct(result))
          else
            ReaderError.wrongJson(s"Can't extract fields from json ${uninitializedFields.mkString("'", "', '", "'")}")

  inline private def buildProduct(values: Array[Any]): Product = new Product:
    override def productArity = values.length
    override def productElement(n: Int) = values(n)
    override def canEqual(that: Any) = true

  val builder: JsonReaderBuilder.type = JsonReaderBuilder


