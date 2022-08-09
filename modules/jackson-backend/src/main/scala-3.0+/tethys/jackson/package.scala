package tethys

import com.fasterxml.jackson.core.{JsonFactory, JsonFactoryBuilder}
import tethys.readers.tokens.{JacksonTokenIteratorProducer, TokenIteratorProducer}
import tethys.writers.tokens.{JacksonTokenWriterProducer, TokenWriterProducer}

package object jackson {
  lazy val defaultJsonFactory: JsonFactory =
    new JsonFactoryBuilder()
      .configure(JsonFactory.Feature.INTERN_FIELD_NAMES, false)
      .build()

  implicit val jacksonTokenWriterProducer: TokenWriterProducer =
    JacksonTokenWriterProducer.default(defaultJsonFactory)

  implicit val jacksonTokenIteratorProducer: TokenIteratorProducer =
    JacksonTokenIteratorProducer.default(defaultJsonFactory)
}
