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
import _root_.scala.xml.{NodeSeq,Text}
import _root_.net.liftweb.http.{RequestVar,S,SHtml}
import _root_.net.liftweb.util.{Box,Empty,Full,Helpers,Log}
import S._
import Helpers._
import SHtml.{hidden, text, select, submit, link}

import _root_.com.jcraft.lift.model._
import _root_.com.jcraft.lift.model.Model._
import _root_.javax.jdo.JDOUserException

import _root_.org.scala_libs.jdo._
import _root_.org.scala_libs.jdo.criterion._

object DataOps{
  import com.google.appengine.api.datastore.{Text => DText}
  import java.lang.Boolean.{FALSE, TRUE}

  val textAdd = Text(?("Add"))
  val textEdit = Text(?("Edit"))
  val textDelete = Text(?("Delete"))

  implicit def jbtos(b:java.lang.Boolean):Boolean =
    if(b == TRUE) true else false

  implicit def stojb(s:String):java.lang.Boolean =
    if(s == "false") FALSE else TRUE

  def resetImage(d:Data){
    val des = Model.withPM{ from(_, classOf[DataEntry])
                                .where(eqC("data", d.id))
                                .orderBy(ascC("date"))
                                .resultList 
    }

    val url = GraphOps.genURL(des, d.cumulative)

    d.image = new DText(url)
    d.save
  }
}

class DataOps {
  import DataOps.{textAdd, textEdit, textDelete}
  import DataOps.stojb

  object dataVar extends RequestVar[Option[Data]](None)
  lazy val data = dataVar.is getOrElse{
    User.currentUser map {user =>
      val d = Data.create
      d.user = user
      d
    } getOrElse {S.error("You need to login"); null}
  }

  def addLink (xhtml : NodeSeq) : NodeSeq = {
    if(User.loggedIn_?) link("add", () => dataVar(None), textAdd)
    else xhtml
  }

  def list (xhtml : NodeSeq) : NodeSeq = {
    val data = Data.findAll

    val uemail = User.currentUser.map(_.email) getOrElse ""
    def isEditable(data:Data) = data.user.email == uemail

    data.flatMap(data =>
      bind("data", xhtml,
           "title" -> link("showData", 
                           () => DataEntryOps.dataVar(Some(data)),
                           Text(data.title)),
           "user" -> Text(data.user.name),
           "edit" -> {if(isEditable(data))
                       link("add", () => dataVar(Some(data)), textEdit)
                      else 
                       textEdit} ,
           "delete" -> {if(isEditable(data))
                         link("list", () => data.delete, textDelete)
                        else 
                          textDelete}))
  }

  def listGraph (xhtml : NodeSeq) : NodeSeq = {
    val data = Data.findAll
    data.flatMap(data =>
      bind("data", xhtml,
           "description" -> Text(data.description),
           "image" -> {
             val src = data.image.getValue
             link("/data/showData",
                  () => DataEntryOps.dataVar(Some(data)),
                  <img src={src} alt={data.title} />)
	   }
          ))
  }

  def is_valid_Data_? (toCheck : Data) : Boolean ={
    List((if (toCheck.title.length == 0) { 
            S.error("You must provide a title"); false 
           } else true),
         (if (toCheck.user == null) {
            S.error("You must select an user"); false
          } else true)
        ).forall(_ == true)
  }

  def add (xhtml : NodeSeq) : NodeSeq = {

    def doAdd () = 
      if (is_valid_Data_?(data)) {
        try{
          data.save
          DataOps.resetImage(data)
          redirectTo("list")
        } 
        catch {
	  case pe : JDOUserException => 
            error("Error adding data"); Log.error("Data add failed", pe)
        }
      }

    val choices = Array("true", "false").map { i => (i -> i) }

    lazy val current = data

    bind("data", xhtml,
         "id" -> hidden(() => dataVar(Some(current))),
         "title" -> text(data.title, data.title=_),
         "description" -> text(data.description, data.description=_),
         "cumulative" -> select(choices, 
                                Full(data.cumulative.toString),
                                data.cumulative = _),
         "user" -> Text(data.user.name),
         "save" -> submit(?("Save"), doAdd))
  }
}
