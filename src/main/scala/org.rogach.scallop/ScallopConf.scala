package org.rogach.scallop

import exceptions._
import scala.util.DynamicVariable

object ScallopConf {
  val rootConf = new DynamicVariable[ScallopConf](null)
  val builders = new DynamicVariable[List[(String,Scallop)]](Nil)
  def cleanUp = {
    ScallopConf.builders.value = Nil
    ScallopConf.rootConf.value = null
  }
}

abstract class ScallopConf(val args: Seq[String] = Nil, protected val commandname: String = "") extends ScallopConfValidations with AfterInit {
  
  if (ScallopConf.rootConf.value == null) {
    ScallopConf.rootConf.value = this
  }
  val rootConfig = ScallopConf.rootConf.value
  
  if (ScallopConf.builders.value.isEmpty) {
    // If this is the root config, init the root builder
    ScallopConf.builders.value = (commandname, Scallop(args)) :: Nil
  } else {
    // if it is the subcommand config, add new builder to the list
    ScallopConf.builders.value = ScallopConf.builders.value :+ (commandname, Scallop(args))
  }
  
  def editBuilder(fn: Scallop => Scallop) {
    val last = ScallopConf.builders.value.last
    ScallopConf.builders.value = ScallopConf.builders.value.init :+ (last._1, fn(last._2))
    builder = ScallopConf.builders.value.last._2
  }
  
  var builder = ScallopConf.builders.value.last._2

  def getName(name: String): String = {
    (ScallopConf.builders.value.map(_._1).mkString("\0") + "\0" + name).stripPrefix("\0")
  }

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
      argName: String = "arg",
      hidden: Boolean = false,
      noshort: Boolean = false)
      (implicit conv:ValueConverter[A]): ScallopOption[A] = {
    editBuilder(_.opt(name, short, descr, default, validate, required, argName, hidden, noshort)(conv))
    new ScallopOption[A](
      getName(name),
      {verified_?; rootConfig.builder.get[A](getName(name))(conv.manifest)},
      {verified_?; rootConfig.builder.isSupplied(getName(name))})
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
    editBuilder(_.props(name, descr, keyName, valueName, hidden)(conv))
    (key:String) => {
      verified_?
      rootConfig.builder(getName(name.toString))(conv.manifest).get(key)
    }
  }

  def propsLong[A](
      name: String,
      descr: String = "",
      keyName: String = "key",
      valueName: String = "value",
      hidden: Boolean = false)
      (implicit conv: ValueConverter[Map[String,A]]):(String => Option[A]) = {
    editBuilder(_.propsLong(name, descr, keyName, valueName, hidden)(conv))
    (key:String) => {
      verified_?
      rootConfig.builder(getName(name))(conv.manifest).get(key)
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
      descr: String = "",
      validate: A => Boolean = (_:A) => true,
      required: Boolean = true,
      default: Option[A] = None,
      hidden: Boolean = false)
      (implicit conv:ValueConverter[A]): ScallopOption[A] = {
    // here, we generate some random name, since it does not matter
    val n = name
    editBuilder(_.trailArg(n, required, descr, default, validate, hidden)(conv))
    new ScallopOption[A](
      getName(name), 
      {verified_?; rootConfig.builder.get[A](n)(conv.manifest)},
      {verified_?; rootConfig.builder.isSupplied(n)})
  }

  /** Add new toggle option definition to this config, and get a holder for it's value.
    *
    * Toggle options are just glorified flag options. For example, if you will ask for a
    * toggle option with name "verbose", it will be invocable in three ways - 
    * "--verbose", "--noverbose", "-v".
    *
    * @param name Name of this option
    * @param default default value for this option
    * @param short Overload the char that will be used as short option name. Defaults to first character of the name.
    * @param noshort If set to true, then this option will not have any short name.
    * @param profix Prefix to name of the option, that will be used for "negative" version of the
                    option. 
    * @param descrYes Description for positive variant of this option.
    * @param descrNo Description for negative variant of this option.
    * @param hidden If set to true, then this option will not be present in auto-generated help.
    */
  def toggle(
      name: String,
      default: Option[Boolean] = None,
      short: Char = 0.toChar,
      noshort: Boolean = false,
      prefix: String = "no",
      descrYes: String = "",
      descrNo: String = "",
      hidden: Boolean = false): ScallopOption[Boolean] = {
    editBuilder(_.toggle(name, default, short, noshort, prefix, descrYes, descrNo, hidden))
    new ScallopOption[Boolean](
      getName(name),
      {verified_?; rootConfig.builder.get[Boolean](name)},
      {verified_?; rootConfig.builder.isSupplied(name)}
    )
  }

  /** Veryfy that this config object is properly configured. */
  def verify {
    try {
      verified = true
      builder.verify
      validations foreach { v =>
        v() match {
          case Right(_) =>
          case Left(err) => throw new ValidationFailure(err)
        }
      }
    } finally {
      ScallopConf.cleanUp
    }
  }
  
  /** Checks that this Conf object is verified. If it is not, throws an exception. */
  def verified_? = {
    if (verified) true
    else {
      ScallopConf.cleanUp
      throw new IncompleteBuildException("It seems you tried to get option value before you constructed all options (maybe you forgot to call .verify method?). Please, move all extraction of values to after 'verify' method in ScallopConf.")
    }
  }
  
  /** In the verify stage, checks that only one or zero of the provided options have values supplied in arguments.
    *
    * @param list list of mutually exclusive options
    */
  def mutuallyExclusive(list: ScallopOption[_]*) {
    editBuilder(_.validationSet { l =>
      if (list.map(_.name).count(l.contains) > 1) Left("There should be only one or zero of the following options: %s" format list.map(_.name).mkString(", "))
      else Right(Unit)
    })
  }
  
  /** In the verify stage, checks that either all or none of the provided options have values supplied in arguments.
    *
    * @param list list of codependent options
    */
  def codependent(list: ScallopOption[_]*) {
    editBuilder(_.validationSet { l =>
      val c = list map (_.name) count (l.contains)
      if (c != 0 && c != list.size) Left("Ether all or none of the following options should be supplied, because they are co-dependent: %s" format list.map(_.name).mkString(", "))
      else Right(Unit)
    })
  }
  
  
  // === some getters for convenience ===
  
  /** Get all data for propety as Map.
    *
    * @param name Propety definition identifier.
    * @return All key-value pairs for this property in a map.
    */
  def propMap[A](name: Char)(implicit m: Manifest[Map[String,A]]) = {
    verified_?
    rootConfig.builder(name.toString)(m)
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
    editBuilder(_.version(v))
  }
  
  /** Add a banner string to option builder.
    *
    * @param b Banner string.
    */
  def banner(b: String) {
    editBuilder(_.banner(b))
  }
  
  /** Add a footer string to this builder.
    *
    * @param f footer string.
    */
  def footer(f: String) {
    editBuilder(_.footer(f))
  }

  /** Explicitly set width of help printout. By default, Scallop tries
    * to determine it from terminal width or defaults to 80 characters.
    */
  def helpWidth(w: Int) {
    editBuilder(_.setHelpWidth(w))
  }
  
  final def afterInit {
    if (ScallopConf.builders.value.size > 1) {
      val b = ScallopConf.builders.value.last
      ScallopConf.builders.value = ScallopConf.builders.value.init
      editBuilder(_.addSubBuilder(b._1, b._2))
      verified = true
    } else {
      try {
        verify
      } finally {
        ScallopConf.cleanUp
      }
    }
  }

}
