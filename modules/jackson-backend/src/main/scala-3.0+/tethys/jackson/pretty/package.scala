package tethys.jackson

import tethys.readers.tokens.TokenIteratorProducer
import tethys.writers.tokens.{JacksonTokenWriterProducer, TokenWriterProducer}

package object pretty {

  implicit val prettyJacksonTokenWriterProducer: TokenWriterProducer =
    JacksonTokenWriterProducer.pretty(defaultJsonFactory)

  implicit val jacksonTokenIteratorProducer: TokenIteratorProducer =
    tethys.jackson.jacksonTokenIteratorProducer
}
