package org.rogach.scallop

import scala.reflect.Manifest

object Scallop {
  def apply(args:Seq[String]):Scallop = new Scallop(args,Nil,Nil,Nil,None,None)
  def apply():Scallop = apply(List())
}

case class Scallop(args:Seq[String], opts:List[OptDef], propts:List[PropDef], trail:List[TrailDef], vers:Option[String], bann:Option[String]) {
  lazy val (pargs,rest) = parseWithRest(args)
  
  type ArgParsed = (Option[String],Option[String],List[String]) // (short, long, args)
  def parseWithRest(args:Seq[String]):(List[ArgParsed], List[List[String]]) = {
    val trailArgs = args.reverse.takeWhile(!_.startsWith("-")).reverse
    val optsRaw = if (trailArgs.size != args.size) parse(args) else Nil
    val optNam = args.reverse.drop(trailArgs.size).headOption 
    optNam match {
      case Some(optName) =>
        val (optConv,required) = if (optName.startsWith("--")) {
          (opts ++ propts).find(_._name == optName.drop(2)).map {o => (o._conv, true)}
          .getOrElse(throw new UnknownOption("Unknown option '%s'" format optName.drop(2)))
        } else {
          propts.find(_.char == optName(1)).map {o => (o._conv, false)}
          .getOrElse(
            opts.map(a => (a,getOptShortName(a))).filter(_._2.isDefined).find(_._2.get == optName.last).map {o => (o._1._conv, true)}
              .getOrElse(throw new UnknownOption("Unknown option '%s'" format optName.drop(1))))
        }
        val rest = parseRest(trailArgs.toList, (optConv, required) :: trail.map(t => (t.conv, t.required))).getOrElse(throw new OptionParseException("Failed to parse the trailing argument list"))
        (parse(args.reverse.drop(trailArgs.size - rest.headOption.map(_.size).getOrElse(0)).reverse), rest.tail)
      case None =>
        val rest = parseRest(trailArgs.toList, trail.map(t => (t.conv, t.required))).getOrElse(throw new OptionParseException("Failed to parse the trailing argument list"))
        (Nil, rest)
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
  def parseRest(args:List[String], convs:List[(ValueConverter[_], Boolean)]):Option[List[List[String]]] = {
    if (convs.isEmpty) {
      if (args.isEmpty) Some(Nil)
      else None
    } else {
      val remainders = convs.head._1.argType match {
        case ArgType.FLAG => List(args)
        case ArgType.SINGLE =>
          if (convs.head._2) if (args.isEmpty) List(Nil) else List(args.tail)
          else if (args.isEmpty) List(Nil) else List(args.tail, args)
        case ArgType.LIST =>
          if (convs.head._2) args.tails.toList.reverse.init
          else args.tails.toList.reverse
      }
      remainders.view.map { rem =>
        val p = args.take(args.size - rem.size)
        if (p.isEmpty && !convs.head._2) {
            val next = parseRest(rem, convs.tail)
            if (next.isDefined) {
              Some(p :: next.get)
            } else None
        } else {
          convs.head._1.parse(List(p)) match {
            case Right(a) if a.isDefined => 
              val next = parseRest(rem, convs.tail)
              if (next.isDefined) {
                Some(p :: next.get)
              } else None
            case _ => None
          }
        }
      }.find(_.isDefined).getOrElse(None)
    }
  }
  
  def opt[A](name:String, short:Char = 0.toChar, descr:String = "", default:Option[A] = None, required:Boolean = false, arg:String = "arg")(implicit conv:ValueConverter[A]):Scallop = {
    val eShort = if (short == 0.toChar) None else Some(short)
    this.copy(opts = opts :+ new OptDef(name, eShort, descr, conv, default, required, arg))
  }
  def props(name:Char,descr:String = "", keyName:String = "key", valueName:String = "value"):Scallop = {
    this.copy(propts = propts :+ new PropDef(name,descr,keyName, valueName))
  }
  def trailArg[A](name:String, required:Boolean = true)(implicit conv:ValueConverter[A]):Scallop = {
    this.copy(trail = trail :+ new TrailDef(name, required, conv))
  }
  
  def version(v:String) = this.copy(vers = Some(v))
  def banner(b:String) = this.copy(bann = Some(b))
  def help:String = (opts ++ propts).sortBy(_._name.toLowerCase).map{ 
    case o:OptDef => o.help(getOptShortName(o))
    case o:PropDef => o.help(Some(o.char))
  }.mkString("\n")
  def args(a:Seq[String]) = this.copy(args = args ++ a)
  
  def get[A](name:String)(implicit m:Manifest[A]):Option[A] = {
    opts.find(_.name == name).map { opt =>
      val sh = getOptShortName(opt)
      if (!(opt.conv.manifest <:< m)) {
        throw new WrongTypeRequest("Requested '%s' instead of '%s'" format (m, opt.conv.manifest))
      }
      opt.conv.parse(pargs.filter(a => a._2.map(opt.name ==)
      .getOrElse(sh.map(a._1.get.head == _).getOrElse(false))).map(_._3)).right.get
      .orElse(opt.default).asInstanceOf[Option[A]]
    }.getOrElse{
      trail.zipWithIndex.find(_._1.name == name).map { case (tr, idx) =>
        if (!(tr.conv.manifest <:< m)) {
          throw new WrongTypeRequest("Requested '%s' instead of '%s'" format (m, tr.conv.manifest))
        }
        tr.conv.parse(List(rest(idx))).right.getOrElse(if (tr.required) throw new MajorInternalException else None).asInstanceOf[Option[A]]
      }.getOrElse(throw new UnknownOption("Unknown option requested: '%s'" format name))
    }
  }
  def apply[A](name:String)(implicit m:Manifest[A]):A = get(name)(m).get
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

  private def getOptShortName(o:OptDef):Option[Char] =
    o.short.orElse {
      val sh = o.name.head
      if ((opts.map(_.short).flatten ++ propts.map(_.char)).contains(sh)) None
      else if (opts.takeWhile(o !=).filter(!_.short.isDefined).map(_.name.head).contains(sh)) None
           else Some(sh)
    }

  def verify = {
    // long options must not clash
    (opts.map(_.name) ++ trail.map(_.name)).groupBy(a=>a).filter(_._2.size > 1)
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

abstract class ArgDef(val _name:String,_short:Option[Char], _descr:String, val _conv:ValueConverter[_]) {
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
case class OptDef(name:String, short:Option[Char], descr:String, conv:ValueConverter[_], default:Option[Any], required:Boolean, arg:String) extends ArgDef(name, short, descr, conv) {
  def argLine(sh:Option[Char]):String = List[Option[String]](sh.map("-" +),Some("--" + name)).flatten.mkString(", ") + "  " + conv.argType.fn(arg)
}
case class PropDef(char:Char, descr:String, keyName:String, valueName:String) extends ArgDef(char.toString, Some(char), descr, propsConverter) {
  def argLine(sh:Option[Char]) = "-%1$s%2$s=%3$s [%2$s=%3$s]..." format (char, keyName, valueName)
}
case class TrailDef(name:String, required:Boolean, conv:ValueConverter[_])

object ArgType extends Enumeration {
  case class V(fn: String => String) extends Val
  val FLAG = V(_ => "")
  val SINGLE = V("<"+_+">")
  val LIST = V("<"+_+">...")
}

