/*
 * Copyright 2009 ymnk, JCraft,Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package com.jcraft.lift.snippet

import _root_ .java.io._
import _root_ .javax.mail.internet.MimeUtility
import _root_.net.liftweb._
import util.{Helpers, Box, Full, Empty, Failure, Log, NamedPF, Props}
import http._
import Helpers._
import _root_ .com.jcraft.lift.model.{User, Data, DataEntry}

object Base64{
  def encode(s:String)={
    val baos = new ByteArrayOutputStream
    MimeUtility.encode(baos, "base64") match{
      case os => 
      os.write(s.getBytes)
      os.flush
      os.close
    }
    baos.toString
  }
}

object TwitterOps{
  import java.text.SimpleDateFormat
  import java.util.Locale

  val df = new SimpleDateFormat("EEE MMM dd HH:mm:ss +0000 yyyy", Locale.US)
  val formatter = new java.text.SimpleDateFormat("yyyyMMdd")
  def today = formatter.format(new java.util.Date).toInt

  def checkMentions(): Box[XmlResponse] = {
    import com.jcraft.lift.model.Twitter
    import com.jcraft.lift.model.Model
    import com.jcraft.lift.model.Model._

    Model.withPM{ from(_, classOf[Twitter]).findOne} match{
      case Some(twitter) if(twitter.user!="" && twitter.passwd!="") =>
        import twitter.{user, passwd}
        import java.net._
        import scala.xml._
        import org.scala_libs.jdo.criterion._

        val url = "http://twitter.com/statuses/mentions.xml"
        val urlConn = new URL(url).openConnection.asInstanceOf[HttpURLConnection]
        urlConn.setRequestProperty("Authorization", 
                                   "Basic "+Base64.encode(user+":"+passwd))
        urlConn.connect();
        urlConn.getResponseCode

        var _lastid = twitter.lastid

        val xml = 
          for (s <- XML.load(urlConn.getInputStream) \ "status" reverse;
               id = (s \ "id" text) if(_lastid < id ) ;
               created_at = s \ "created_at" text ;
               text = s \ "text" text ;
               screen_name = s \ "user" \ "screen_name" text
               ) yield {

             _lastid = id
            val users = Model.withPM{ from(_, classOf[User])
                                      .where(eqC("twitter", screen_name))
                                      .resultList}

            val Array(_, dataset, dataEntry, _date@_*) = text.split(" ")
            val date  = if(_date.length==0) today
                        else try{ _date(0).toInt }catch{ case e => today } 

            users.foreach{ u =>
              u.data.toArray.foreach{ case d:Data =>
                if(d.title == dataset){
                  val de = DataEntry.create
                  de.data = d.id
                  de.date = date
                  de.dataEntry = dataEntry
                  de.save
                }
              }
            } 

            <div>
              <span>{screen_name}</span>
              <span>{text}</span>
              <span>{created_at}</span>
            </div>
        }

        if(_lastid != twitter.lastid){
          twitter.lastid = _lastid
          Model.withPM{_.makePersistent(twitter)}
        }

        Full(XmlResponse(<div>{xml}</div>))

      case _ => 
        Full(XmlResponse(<div></div>))
    }

  }
}

object GraphOps{

  def maxf(a:Float, b:Float) = if(a>b) a else b
  def minf(a:Float, b:Float) = if(a<b) a else b

  def genURL(del:List[DataEntry], cumulative: Boolean):String={

    if(del.length == 0){
      return ""
    }

    val data = if(cumulative){
      val cumulative = new Array[Float](del.length)
      def loop(n:Float, l:List[DataEntry]){
        l match{
          case Nil => cumulative(del.length-1) = n
          case head::tail =>
            cumulative(del.length-(l.length+1))=n
            loop(n+head.dataEntry.toFloat, tail)
        }
      }
      loop(del.head.dataEntry.toFloat, del.tail)
      cumulative
    }
    else{
      del.map(_.dataEntry.toFloat)
    }

    val dseq = data.mkString(",")
    val max = if(cumulative)  data(data.length-1) 
              else data.foldLeft(data(0)){maxf(_, _)}
    val min = if(cumulative)  0
              else data.foldLeft(data(0)){minf(_,_)}

    "http://chart.apis.google.com/chart?cht=lc&chs=250x100&chd=t:"+dseq+"&chds="+min+","+max+"&chxt=r&chxl=0:|"+min+"|"+max
  }
}
