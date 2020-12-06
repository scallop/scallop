package org.rogach.scallop

/** An enumeration of possible arg types by number of arguments they can take. */
object ArgType {

  /** Custom class for enumeration type.
    * @param fn Transformation from option name to option's args definition in help. */
  sealed abstract class V(val fn: String => String)

  /** Option takes no argument. At all. It is either there or it isn't. */
  case object FLAG extends V(_ => "")

  /** Option takes only one argument. (for example, .opt[Int]) */
  case object SINGLE extends V("<" + _ + ">")

  /** Option takes any number of arguments. (.opt[List[Int]]) */
  case object LIST extends V("<" + _ + ">...")
}
