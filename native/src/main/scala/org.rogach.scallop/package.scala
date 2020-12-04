package org.rogach

import java.io.File

package object scallop extends DefaultConverters {
  implicit val fileConverter: ValueConverter[File] =
    singleArgConverter(new File(_), PartialFunction.empty)  // Note: important to provide default arg (Dotty)
  implicit val fileListConverter: ValueConverter[List[File]] =
    listArgConverter(new File(_))
}
