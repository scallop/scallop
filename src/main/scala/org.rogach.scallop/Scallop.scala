package org.rogach.scallop

import scala.reflect.Manifest

object Scallop {
  def apply(args:Seq[String]):Scallop = new Scallop(args,List(),List(),None,None)
  def apply():Scallop = apply(List())
}

case class Scallop(args:Seq[String], opts:Seq[OptDef], propts:Seq[PropDef], vers:Option[String], bann:Option[String]) {
  lazy val (pargs,rest) = parseWithRest(args)
  type ArgParsed = (Option[String],Option[String],List[String])
  def parseWithRest(args:Seq[String]):(List[ArgParsed],Seq[String]) = {
    val (tail, init) = args.reverse.span(!_.startsWith("-"))
    if (init.isEmpty) (parse(args),Nil)
    else if (init.head.startsWith("--")) {
      opts.find(_.name == init.head.drop(2)) match {
        case Some(opt) =>
          opt.argType match {
            case ArgType.FLAG => (parse(args.reverse.drop(tail.length).reverse), tail.reverse)
            case ArgType.SINGLE => (parse(args.reverse.drop(tail.length - 1).reverse), tail.reverse.drop(1))
            case ArgType.LIST => (parse(args), Nil)
          }
        case None => (parse(args), Nil) // let's throw all errors in verify stage
      }
    } else {
      opts.map(a => (a,getOptShortName(a))).filter(_._2.isDefined).find(_._2.get == init.head.last) match {
        case Some((opt, char)) =>
          opt.argType match {
            case ArgType.FLAG => (parse(args.reverse.drop(tail.length).reverse), tail.reverse)
            case ArgType.SINGLE => (parse(args.reverse.drop(tail.length - 1).reverse), tail.reverse.drop(1))
            case ArgType.LIST => (parse(args), Nil)
          }
        case None => // maybe it's a property?
          propts.find(_.char == init.head(1)) match {
            case Some(prop) =>
              val restSize = tail.size - tail.reverse.takeWhile(_.contains('=')).size
              (parse(args.reverse.drop(restSize).reverse), tail.take(restSize).reverse)
            case None => (parse(args), Nil) // let's throw all errors in verify stage
          }
      }
    }
  }
  def parse(args:Seq[String]):List[ArgParsed] = {
    args.toList match {
      case a :: rest if a.startsWith("--") =>
        (None,Some(a.drop(2)),rest.takeWhile(!_.startsWith("-"))) :: 
          parse(rest.dropWhile(!_.startsWith("-")))
      case a :: rest if a.startsWith("-") =>
        if (propts.find(_.char == a(1)).isDefined) {
          (Some(a(1).toString), None, (a.drop(2) +: rest.takeWhile(!_.startsWith("-"))).filter(_.size > 0)) ::
          parse(rest.dropWhile(!_.startsWith("-")))
        } else {
          a.drop(1).init.map(i => (Some(i.toString), None, List[String]())).toList :::
          List((Some(a.last.toString),None,rest.takeWhile(!_.startsWith("-")))) :::
          parse(rest.dropWhile(!_.startsWith("-")))
        }
      case Nil => List()
      case a => throw new OptionParseException("Failed to parse options: " + a.mkString(" "))// there should be options!
    }
  }
  def opt[A](name:String, short:Char = 0.toChar, descr:String = "", default:Option[A] = None, required:Boolean = false, arg:String = "arg")(implicit conv:ValueConverter[A], m:Manifest[A]):Scallop = {
    val eShort = if (short == 0.toChar) None else Some(short)
    val argType =
      if (m <:< implicitly[Manifest[Boolean]]) ArgType.FLAG
      else if (m <:< implicitly[Manifest[List[_]]]) ArgType.LIST
      else ArgType.SINGLE
    this.copy(opts = opts :+ new OptDef(name,eShort,descr,conv,m,default,required,arg, argType))
  }
  def props(name:Char,descr:String = "", keyName:String = "key", valueName:String = "value"):Scallop = {
    this.copy(propts = propts :+ new PropDef(name,descr,keyName, valueName))
  }
  def version(v:String) = this.copy(vers = Some(v))
  def banner(b:String) = this.copy(bann = Some(b))
  def help:String = (opts ++ propts).sortBy(_._name.toLowerCase).map{ 
    case o:OptDef => o.help(getOptShortName(o))
    case o:PropDef => o.help(Some(o.char))
  }.mkString("\n")
  def args(a:Seq[String]) = this.copy(args = args ++ a)
  def get[A](name:String)(implicit m:Manifest[A]):Option[A] = {
    val opt = opts.find(_.name == name).getOrElse(throw new UnknownOption("Unknown option requested: '%s'" format name))
    val sh = getOptShortName(opt)
    if (!(m <:< opt.m)) {
      throw new WrongTypeRequest("")
    }
    opt.conv.parse(pargs.filter(a => a._2.map(opt.name ==)
    .getOrElse(sh.map(a._1.get.head == _).getOrElse(false))).map(_._3)).right.get
    .orElse(opt.default).asInstanceOf[Option[A]]
  }
  def prop(name:Char, key:String):Option[String] = {
    pargs.filter(_._1 == Some(name.toString)).flatMap { p =>
      val rgx = """([^=]+)=(.*)""".r
      p._3.collect {
        case rgx(key, value) => (key, value)
      }
    }.find(_._1 == key).map(_._2)
  }
  def propMap(name:Char):Map[String,String] = {
    pargs.filter(_._1 == Some(name.toString)).flatMap { p =>
      val rgx = """([^=]+)=(.*)""".r
      p._3.collect {
        case rgx(key, value) => (key, value)
      }
    }.toMap
  }

  def apply[A](name:String)(implicit m:Manifest[A]):A = get(name)(m).get
  private def getOptShortName(o:OptDef):Option[Char] =
    o.short.orElse {
      val sh = o.name.head
      if ((opts.map(_.short).flatten ++ propts.map(_.char)).contains(sh)) None
      else if (opts.takeWhile(o !=).filter(!_.short.isDefined).map(_.name.head).contains(sh)) None
           else Some(sh)
    }

  def verify = {
    // long options must not clash
    opts.groupBy(_.name).filter(_._2.size > 1)
    .foreach(a => throw new IdenticalOptionNames("Long option name '%s' is not unique" format a._1))
    // short options must not clash
    (opts.map(_.short).flatten ++ propts.map(_.char)).groupBy(a=>a).filter(_._2.size > 1)
    .foreach(a => throw new IdenticalOptionNames("Short option name '%s' is not unique" format a._1))
    
    if (pargs.find(_._2 == Some("help")).isDefined) {
      vers.foreach(println)
      bann.foreach(println)
      println(help)
      sys.exit(0)
    }
    if (vers.isDefined && pargs.find(_._2 == Some("version")).isDefined) {
      println("version")
      sys.exit(0)
    }
    // check that there are no garbage options
    pargs.foreach { arg =>
      if (!arg._2.map(n => opts.find(_.name == n).isDefined)
          .getOrElse((opts.map(o => o.short.getOrElse(o.name.head)) ++ propts.map(_.char)).find(arg._1.get.head ==).isDefined))
        throw new UnknownOption("Unknown option: %s" format arg._1.getOrElse(arg._2.get))
    }
    
    opts.foreach { o => 
      val sh = getOptShortName(o)
      val params = pargs.filter(a => a._2.map(o.name ==).getOrElse(sh.map(a._1.get.head == _).getOrElse(false))).map(_._3)
      val res = o.conv.parse(params)
      if (res.isLeft) throw new WrongOptionFormat("Wrong format for option '%s': %s" format (o.name, params.map(_.mkString).mkString(" ")))
      if (o.required && !res.right.get.isDefined && !o.default.isDefined) throw new RequiredOptionNotFound("Required option '%s' not found" format o.name)
    }
    this
  }
}

abstract class ArgDef(val _name:String,_short:Option[Char], _descr:String) {
  def argLine(sh:Option[Char]):String
  def help(sh:Option[Char]):String = {
    var text = List[String]("")
    _descr.split(" ").foreach { w =>
      if (text.last.size + 1 + w.size <= 76) {
        text = text.init :+ (text.last + w + " ")
      } else if (text.last.size + w.size <= 76) {
        text = text.init :+ (text.last + w)
      } else text :+= w
    }
    (argLine(sh) + "\n" + text.map("    " +).mkString("\n")).trim
  }
  
}
case class OptDef(name:String, short:Option[Char], descr:String, conv:ValueConverter[_],m:Manifest[_], default:Option[Any], required:Boolean, arg:String, argType:ArgType.V) extends ArgDef(name, short, descr) {
  def argLine(sh:Option[Char]):String = List[Option[String]](sh.map("-" +),Some("--" + name)).flatten.mkString(", ") + "  " + argType.fn(arg)
}

case class PropDef(char:Char, descr:String, keyName:String, valueName:String) extends ArgDef(char.toString, Some(char), descr) {
  def argLine(sh:Option[Char]) = "-%1$s%2$s=%3$s [%2$s=%3$s]..." format (char, keyName, valueName)
}

object ArgType extends Enumeration {
  case class V(fn: String => String) extends Val
  val FLAG = V(_ => "")
  val SINGLE = V("<"+_+">")
  val LIST = V("<"+_+">...")
}

