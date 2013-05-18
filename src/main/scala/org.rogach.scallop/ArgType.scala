package org.rogach.scallop

/** An enumeration of possible arg types by number of arguments they can take. */
object ArgType extends Enumeration {

  /** Custom class for enumeration type.
    * @param fn Transformation from option name to option's args definition in help. */
  case class V(fn: String => String) extends Val

  /** Option takes no argument. At all. It is either there or it isn't. */
  val FLAG = V(_ => "")

  /** Option takes only one argument. (for example, .opt[Int]) */
  val SINGLE = V("<" + _ + ">")

  /** Option takes any number of arguments. (.opt[List[Int]]) */
  val LIST = V("<" + _ + ">...")
}

