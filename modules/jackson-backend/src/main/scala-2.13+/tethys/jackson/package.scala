package tethys

import com.fasterxml.jackson.core.{JsonFactory, JsonFactoryBuilder}
import tethys.readers.tokens.{JacksonTokenIteratorProducer, TokenIterator, TokenIteratorProducer}
import tethys.readers.{FieldName, ReaderError}
import tethys.writers.tokens.{JacksonTokenWriterProducer, TokenWriter, TokenWriterProducer}

import java.io.{Reader, Writer}

package object jackson {
  lazy val defaultJsonFactory: JsonFactory =
    new JsonFactoryBuilder()
      .configure(JsonFactory.Feature.INTERN_FIELD_NAMES, false)
      .build()

  implicit def jacksonTokenWriterProducer(implicit jsonFactory: JsonFactory = defaultJsonFactory): TokenWriterProducer =
    JacksonTokenWriterProducer.default(jsonFactory)


  implicit def jacksonTokenIteratorProducer(implicit jsonFactory: JsonFactory = defaultJsonFactory): TokenIteratorProducer =
    JacksonTokenIteratorProducer.default(jsonFactory)
}