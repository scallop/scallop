package org.rogach.scallop

/** A class to hold a reference to not-yet-computed option values.
  * Basically, this is a lazy option - it batches up all operations,
  * and evaluates the value only as the last resort.
  * @param fn source of the actual value 
  * @param supplied function, that is capable of testing whether the value was explicitly
  *                 found in argument list.
  */
class ScallopOption[A](
  val name:String,
  fn: => Option[A],
  supplied: => Boolean
) { opt => 
  
  /** Retreive the underlying value as an option */
  def get = fn
  
  /** Retreive the underlying value. Use only if you are completely sure that there is a value. */
  def apply() = fn.get
  
  /** Tests whether the underlying value was explicitly supplied by user.
    */
  def isSupplied = supplied
  
  /** Returns ScallopOption, that contains the result of applying ```pf```
    * to the value, if this option is non-empty and pf is defined for that value.
    * Returns empty ScallopOption otherwise.
    * @param pf the partial function
    */
  def collect[B](pf: PartialFunction[A,B]) =
    new ScallopOption(name, opt.get.collect(pf), supplied)
  
  /** Returns ScallopOption, that contains the value if applying
    * predicate p to this value returned true. No value otherwise.
    * @param p the predicate used for testing
    */
  def filter(p: A=>Boolean) =
    new ScallopOption(name, opt.get.filter(p), supplied)

  /** Returns ScallopOption, that contains the value if applying
    * predicate p to this value returned false. No value otherwise.
    * @param p the predicate used for testing
    */
  def filterNot(p: A=>Boolean) =
    new ScallopOption(name, opt.get.filterNot(p), supplied)
    
  /** Returns ScallopOption, that contains the result of applying
    * ```f``` to this option's value, if this option is non-empty.
    * Returns ScallopOption with no value otherwise.
    * @param f the function to apply
    */
  def map[B](f: A=>B) = 
    new ScallopOption(name, opt.get.map(f), supplied)
    
  /** Returns ScallopOption with this value if it is non-empty,
    * or with the value of the alternative option. If it is 
    * empty too, the resulting ScallopOption will contain None.
    * @param alternative the alternative expression
    */
  def orElse[B >: A](alternative: =>Option[B]) =
    new ScallopOption(name, opt.get.orElse(alternative), supplied)
    
  /** A convenience method to check whether the underlying option
    * is defined. Just an alias for opt.get.isDefined.
    */
  def isDefined = get.isDefined
  
  /** A convenience method to check whether the underlying option is
    * empty. Just an alias for !opt.isDefined.
    */
  def isEmpty = !isDefined
    
  override def toString = opt.get match {
    case Some(x) => "ScallopSome(%s)" format x
    case None => "ScallopNone"
  }
}
