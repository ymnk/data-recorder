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

import _root_.java.text.{ParseException,SimpleDateFormat}
import _root_.scala.xml.{NodeSeq,Text,Group}
import _root_.net.liftweb.http.{RequestVar,SessionVar, S,SHtml,FileParamHolder}
import _root_.net.liftweb.util.{Box,Empty,Full,Helpers,Log}
import S._
import SHtml._
import util._
import Helpers._

import _root_.com.jcraft.lift.model._
import _root_.com.jcraft.lift.model.Model._
import _root_.javax.jdo.JDOUserException

import _root_.org.scala_libs.jdo._
import _root_.org.scala_libs.jdo.criterion._
import _root_.java.util.Date
import com.google.appengine.api.datastore.{Text => DText}

object DataEntryOps {
  object dataVar extends SessionVar[Option[Data]](None)

  private val formatter = new java.text.SimpleDateFormat("yyyyMMdd")
  def today = formatter.format(new Date).toInt
}

class DataEntryOps {
  import DataEntryOps._
 
  lazy val data = dataVar.is

  object deVar extends RequestVar[Option[DataEntry]](None)
  lazy val dataEntry = deVar.is getOrElse {
    new DataEntry() match{
      case de => 
        de.data = data.get.id
        de.date = today
        de
    }
  }

  def list (xhtml : NodeSeq) : NodeSeq = {
    val dataentries = {
      val where = data match {
                      case None => Nil
                      case Some(d) => List(eqC("data", d.id))
      }
      Model.withPM{ from(_, classOf[DataEntry])
           .where(where:_*)
           .orderBy(descC("date"))
           .resultList 
      }
    }

    val isEditable = User.currentUser map{ u =>
      data map (_.user.id == u.id) getOrElse false
    } getOrElse false

    val textEdit = Text(?("Edit"))
    val textDelete = Text(?("Delete"))
    import SHtml.link
    dataentries.flatMap(de =>
      bind("dataentry", xhtml,
           "data" -> Text(de.data),
           "date" -> Text(de.date.toString),
           "dataEntry" -> Text(de.dataEntry),
           "edit" -> {if(isEditable)
                        link("add", () => deVar(Some(de)), textEdit)
                      else textEdit },
           "delete" -> {if(isEditable)
                          link("showData", 
                               () => {
                                 Model.withPM{ _.deletePersistent(de)}
                                 DataOps.resetImage(data.get)
                               },
                               textDelete)
                        else textDelete}))
  }
 
  def add (xhtml : NodeSeq) : NodeSeq = {
    def is_valid_DataEntry_? (de : DataEntry) : Boolean ={
      List(true).forall(_ == true)
    }
    def doAdd () = 
      if (is_valid_DataEntry_?(dataEntry)) {
        try{
          Model.withPM{ _.makePersistent(dataEntry) }
          DataOps.resetImage(data.get)
          redirectTo("showData")
        } 
        catch {
	  case pe : JDOUserException => 
            error("Error adding data"); Log.error("DataEntry add failed", pe)
        }
      }

    val isEditable = User.currentUser map{ u =>
      data map (_.user.id == u.id) getOrElse false
    } getOrElse false

    if(isEditable){
      import SHtml.{hidden, text, select, submit}
      bind("dataentry", xhtml,
           "id" -> hidden(() => deVar(Some(dataEntry))),
           "date" -> text(dataEntry.date.toString, 
                          (s)=>dataEntry.date=s.toInt),
           "dataEntry" -> text(dataEntry.dataEntry, dataEntry.dataEntry=_),
           "save" -> submit(?("Save"), doAdd))
    }
    else{
      <div></div>
    }
  }

  def graph(xhtml: Group): NodeSeq ={
    val image = data map { d =>
      val src = d.image.getValue
      if(src == "" ) <div></div>
      else <img src={src} alt={d.title} />
    } getOrElse (<div></div>)

    val description = data map ( _.description ) getOrElse ""

    bind("graph", xhtml,
         "title" -> Text(description),
         "image" -> image)
  }

  private object theUpload extends RequestVar[Box[FileParamHolder]](Empty)

  def upload(xhtml: Group): NodeSeq ={

    def procUpload(f:FileParamHolder){
      val pattern = """"?([^"]+)"?,"?([^"]+)"?""".r
      Model.withPM{ pm => 
      for (line <- scala.io.Source.fromBytes(f.file).getLines){
        line.stripLineEnd match {
          case pattern(date, dataEntry) => 
            val de = new DataEntry
            de.data = data.get.id
            de.date = date.split("-").mkString.split("/").mkString.toInt
            de.dataEntry = dataEntry
            pm.makePersistent(de)
          case _ =>
        }
      }
      }    
      DataOps.resetImage(data.get)
    }

    val isEditable = User.currentUser map{ u =>
      data map (_.user.id == u.id) getOrElse false
    } getOrElse false

    if(isEditable){
      if (S.get_?) 
        bind("ul", chooseTemplate("choose", "get", xhtml),
             "file_upload" -> fileUpload(ul => {
                                           procUpload(ul);
                                           theUpload(Full(ul));
                                           })
        )
      else 
         bind("ul", chooseTemplate("choose", "post", xhtml),
              "file_name" -> theUpload.is.map(v => Text(v.fileName)),
              "mime_type" -> theUpload.is.map(v => Box.legacyNullTest(v.mimeType).map(Text).openOr(Text("No mime type supplied"))),
              "length" -> theUpload.is.map(v => Text(v.file.length.toString)),
              "md5" -> theUpload.is.map(v => Text(hexEncode(md5(v.file))))
         )
    }
    else{
      <div></div>
    }
  }
}
