package org.rogach.scallop

import exceptions._

abstract class ScallopConf(val args: Seq[String]) extends ScallopConfValidations with AfterInit {
  
  var builder = Scallop(args)
  var verified = false
  
  /** Add a new option definition to this config and get a holder for the value.
    *
    * @param name Name for new option, used as long option name in parsing, and for option identification.
    * @param short Overload the char that will be used as short option name. Defaults to first character of the name.
    * @param descr Description for this option, for help description.
    * @param default Default value to use if option is not found in input arguments (if you provide this, you can omit the type on method).
    * @param required Is this option required? Defaults to false.
    * @param arg The name for this ortion argument, as it will appear in help. Defaults to "arg".
    * @param noshort If set to true, then this option does not have any short name.
    * @param conv The converter for this option. Usually found implicitly.
    * @return A holder for parsed value
    */
  def opt[A](
      name: String,
      short: Char = 0.toChar,
      descr: String = "",
      default: Option[A] = None,
      validate: A => Boolean = (_:A) => true,
      required: Boolean = false,
      arg: String = "arg",
      hidden: Boolean = false,
      noshort: Boolean = false)
      (implicit conv:ValueConverter[A]): ScallopOption[A] = {
    builder = builder.opt(name, short, descr, default, validate, required, arg, hidden, noshort)(conv)
    new ScallopOption[A](
      name,
      {verified_?; builder.get[A](name)(conv.manifest)},
      {verified_?; builder.isSupplied(name)})
  }              

  /** Add new property option definition to this config object, and get a handle for option retreiving.
    * 
    * @param name Char, that will be used as prefix for property arguments.
    * @param descr Description for this property option, for help description.
    * @param keyName Name for 'key' part of this option arg name, as it will appear in help option definition. Defaults to "key".
    * @param valueName Name for 'value' part of this option arg name, as it will appear in help option definition. Defaults to "value".
    * @return A holder for retreival of the values.
    */
  def props[A](
      name: Char,
      descr: String = "",
      keyName: String = "key",
      valueName: String = "value",
      hidden: Boolean = false)
      (implicit conv: ValueConverter[Map[String,A]]):(String => Option[A]) = {
    builder = builder.props(name, descr, keyName, valueName, hidden)(conv)
    (key:String) => {
      verified_?
      builder.prop(name, key)(conv.manifest)
    }
  }
  
  /** Add new trailing argument definition to this config, and get a holder for it's value.
    *
    * @param name Name for new definition, used for identification.
    * @param required Is this trailing argument required? Defaults to true.
    * @param default If this argument is not required and not found in the argument list, use this value.
    */
  def trailArg[A](
      name: => String = ("trailArg " + (1 to 10).map(_ => (97 to 122)(math.random * 26 toInt).toChar).mkString),
      required: Boolean = true,
      default: Option[A] = None)
      (implicit conv:ValueConverter[A]): ScallopOption[A] = {
    // here, we generate some random name, since it does not matter
    val n = name
    builder = builder.trailArg(n, required, default)(conv)
    new ScallopOption[A](
      name, 
      {verified_?; builder.get[A](n)(conv.manifest)},
      {verified_?; builder.isSupplied(n)})
  }
  
  /** Veryfy that this config object is properly configured. */
  def verify {
    verified = true
    builder.verify
    validations foreach { v =>
      v() match {
        case Right(_) =>
        case Left(err) => throw new ValidationFailure(err)
      }
    }
  }
  
  /** Checks that this Conf object is verified. If it is not, throws an exception. */
  def verified_? = {
    if (verified) true
    else throw new IncompleteBuildException("It seems you tried to get option value before you constructed all options (maybe you forgot to call .verify method?). Please, move all extraction of values to after 'verify' method in ScallopConf.")
  }
  
  /** In the verify stage, checks that only one or zero of the provided options have values supplied in arguments.
    *
    * @param list list of mutually exclusive options
    */
  def mutuallyExclusive(list: ScallopOption[_]*) {
    builder = builder.validationSet { l =>
      if (list.map(_.name).count(l.contains) > 1) Left("There should be only one or zero of the following options: %s" format list.map(_.name).mkString(", "))
      else Right(Unit)
    }
  }
  
  /** In the verify stage, checks that either all or none of the provided options have values supplied in arguments.
    *
    * @param list list of codependent options
    */
  def codependent(list: ScallopOption[_]*) {
    builder = builder.validationSet { l =>
      val c = list map (_.name) count (l.contains)
      if (c != 0 && c != list.size) Left("Ether all or none of the following options should be supplied, because they are co-dependent: %s" format list.map(_.name).mkString(", "))
      else Right(Unit)
    }
  }
  
  
  // === some getters for convenience ===
  
  /** Get all data for propety as Map.
    *
    * @param name Propety definition identifier.
    * @return All key-value pairs for this property in a map.
    */
  def propMap[A](name: Char)(implicit m: Manifest[Map[String,A]]) = {
    verified_?
    builder.propMap(name)(m)
  }

  /** Get summary of current parser state.
    *
    * @return a list of all options in the builder, and corresponding values for them.
    */
  def summary = {
    verified_?
    builder.summary
  }
  
  /** Add a version string to option builder.
    *
    * @param v Version string.
    */
  def version(v: String) {
    builder = builder.version(v)
  }
  
  /** Add a banner string to option builder.
    *
    * @param b Banner string.
    */
  def banner(b: String) {
    builder = builder.banner(b)
  }
  
  /** Add a footer string to this builder.
    *
    * @param f footer string.
    */
  def footer(f: String) {
    builder = builder.footer(f)
  }
  
  final def afterInit {
    verify
  }
}
