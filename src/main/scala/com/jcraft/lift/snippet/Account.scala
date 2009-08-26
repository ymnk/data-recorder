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

import scala.xml.{NodeSeq,Text}

import net.liftweb.http.{S, SHtml}
import net.liftweb.util.{Helpers, Log, Empty}
import S._
import Helpers._

import com.google.appengine.api.users.{UserService, UserServiceFactory}
import com.jcraft.lift.model.User

class Account {
  def render = {
    val userService = UserServiceFactory.getUserService

    User.currentUser map {user =>
      val logout = userService.createLogoutURL("/")
      <span>
        Hello, {user.name} &nbsp; 
        <a href="/users/edit">Settings</a> &nbsp; 
        <a href={logout}>Log out</a>
      </span>
    } getOrElse {
      SHtml.link("/",
                 () => {
                   val myuri = S.request.open_!.request.uri
                   S.redirectTo(userService.createLoginURL(myuri))
                 },
                 Text(?("Log in")))
    }
  } 
}
