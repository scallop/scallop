package org.rogach

import java.io.File

package object scallop extends DefaultConverters {
  implicit val fileConverter: ValueConverter[File] =
    singleArgConverter(new File(_))
}
